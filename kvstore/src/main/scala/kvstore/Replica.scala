package kvstore

import scala.concurrent.duration._

import akka.actor._
import akka.actor.SupervisorStrategy.{Restart, Stop}
import kvstore.Arbiter._

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }

  case object RetryPersist
  case class TimeOut(id: Long)

  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import context.dispatcher
  import kvstore.Persistence._
  import kvstore.Replica._
  import kvstore.Replicator._

  override def supervisorStrategy: SupervisorStrategy = AllForOneStrategy() {
    case _: PersistenceException => Restart
    case _: ActorKilledException => Stop
  }

  // Retry all not acknowledged persist command every 100 millis
  context.system.scheduler.schedule(100.millis, 100.millis, context.self, RetryPersist)

  // Join before starting
  val beforeStart = arbiter ! Join

  // Persistence actor
  val persistence = context.actorOf(persistenceProps)

  // Key-Value Store
  var kv = Map.empty[String, String]

  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]

  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  // A list of persist commands which haven't been acknowledged
  var notAcked = Map.empty[Long, (ActorRef, Option[Persist], Set[ActorRef])]

  // Keeps the version and sequence number
  var expectedVersion = 0L
  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

  /**
   * Actor's receive entry point
   * @return
   */
  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  /**
   * Sends a persist command to the persistence actor and records it as not acknowledged
   * @param id
   * @param key
   * @param value
   */
  def persist(id: Long, key: String, value: Option[String]) = {
    val command = Persist(key, value, id)
    notAcked += id -> (sender, Some(command), Set.empty[ActorRef])
    persistence ! command
  }

  /**
   * Persistence handler for primary replica, in addition to sending the persistence message to the persistence actor, it
   * sends replication requests to all secondary replicas
   * @param id
   * @param key
   * @param value
   * @return
   */
  def persistPrimary(id: Long, key: String, value: Option[String]) = {
    persist(id, key, value)
    replicators foreach (_ ! Replicate(key, kv.get(key), id))
    context.system.scheduler.scheduleOnce(1000.millis, context.self, TimeOut(id))
  }

  /**
   * Checks the status of the persistence request and acknowledges the operation in case of success
   * @param id
   * @return
   */
  def checkAckStatus(id: Long) = {
    notAcked.get(id) map {entry =>
      val (client, persist, reps) = entry
      persist match {
        case None if replicators forall (reps.contains(_)) => client ! OperationAck(id); notAcked -= id
        case _ =>
      }
    }
  }

  /**
   * The receive handler of primary replica
   */
  val leader: Receive = {
    case Insert(key, value, id) => {
      kv += key -> value
      persistPrimary(id, key, Some(value))
    }
    case Remove(key, id)  => {
      kv -= key
      persistPrimary(id, key, None)
    }
    case Get(key, id) => sender ! GetResult(key, kv.get(key), id)
    case RetryPersist => {
      notAcked foreach {
        case (_, (_, command, _)) => command.map(persistence ! _)
      }
    }
    case Persisted(key, id) => {
      notAcked.get(id) map {
        case (client, _, replicas) => {
          notAcked += id -> (client, None, replicas)
          checkAckStatus(id)
        }
      }
    }
    case TimeOut(id) => {
      checkAckStatus(id)
      notAcked.get(id) map {
        case (client, _, _) => client ! OperationFailed(id)
      }
      notAcked -= id
    }
    case Replicated(key, id) => {
      notAcked.get(id) map {
        case (client, command, replicas) => {
          notAcked += id -> (client, command, replicas + sender)
          checkAckStatus(id)
        }
      }
    }
    case Replicas(replicas) =>
      var updatedReplicators = Set.empty[ActorRef]
      var updateSecondaries = Map.empty[ActorRef, ActorRef]
      val notMeReplicas = replicas filter (_ != self)
      notMeReplicas map { replica =>
        val replicator = secondaries.getOrElse(replica, context.actorOf(Props(classOf[Replicator], replica)))
        if (!secondaries.contains(replica)) {
          kv.foreach {
            case (key, value) => replicator ! Replicate(key, Some(value), nextSeq)
          }
        }
        updatedReplicators += replicator
        replicators -= replicator
        updateSecondaries += (replica -> replicator)
      }
      replicators foreach(context.stop)
      replicators = updatedReplicators
      secondaries = updateSecondaries
      notAcked.keySet.foreach(checkAckStatus)
  }

  /**
   * The receive handler of secondary replicas
   */
  val replica: Receive = {
    case Get(key, id) => sender ! GetResult(key, kv.get(key), id)
    case Snapshot(key, value, seq) => {
      if (seq == expectedVersion) {
        value match {
          case None => kv -= key
          case Some(value) => kv += key -> value
        }
        persist(seq, key, value)
      }
      else if (seq < expectedVersion) sender ! SnapshotAck(key, seq)
    }
    case Persisted(key, id) => {
      notAcked.get(id) map {
        case (requester, persist, _) => {
          persist.map(cmd => requester ! SnapshotAck(key, cmd.id))
          expectedVersion += 1
        }
      }
      notAcked -= id
    }
    case RetryPersist => {
      notAcked foreach {
        case (_, (_, command, _)) => command.map(persistence ! _)
      }
    }
  }
}