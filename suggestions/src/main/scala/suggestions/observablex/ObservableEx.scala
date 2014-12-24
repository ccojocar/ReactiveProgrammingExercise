package suggestions
package observablex

import rx.lang.scala.Observable
import rx.lang.scala.subjects.ReplaySubject

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Failure}

object ObservableEx {

  /** Returns an observable stream of values produced by the given future.
   * If the future fails, the observable will fail as well.
   *
   * @param f future whose values end up in the resulting observable
   * @return an observable completed after producing the value of the future, or with an exception
   */
  def apply[T](f: Future[T])(implicit execContext: ExecutionContext): Observable[T] = {
    val subject = ReplaySubject[T]()
    f onComplete {
      case Failure(e) => { subject.onError(e) }
      case Success(v) => { subject.onNext(v); subject.onCompleted() }
    }
    subject
  }
}