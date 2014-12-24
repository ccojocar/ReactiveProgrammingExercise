package kvstore

import scala.concurrent.duration._

import akka.actor.{Actor, ActorRef, Props}

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  case object RetrySnap

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import context.dispatcher
  import kvstore.Replicator._

  // Retry all not acknowledged every 100 millis
  context.system.scheduler.schedule(100.millis, 100.millis, context.self, RetrySnap)
  
  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]
  
  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }
  
  def receive: Receive = {
    case rep @ Replicate(key, value, id) => {
      val seq = nextSeq
      acks += seq -> (sender, rep)
      replica ! Snapshot(key, value, seq)
    }
    case SnapshotAck(key, seq) => {
      acks.get(seq) map {
        case (primary, command) => primary ! Replicated(key, command.id)
      }
      acks -= seq
    }
    case RetrySnap => {
      acks foreach {
        case (seq, (_, replicate)) => replica ! Snapshot(replicate.key, replicate.valueOption, seq)
      }
    }
  }
}