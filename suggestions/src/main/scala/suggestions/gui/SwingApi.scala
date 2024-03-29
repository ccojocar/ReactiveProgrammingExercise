package suggestions
package gui

import rx.lang.scala.Observable
import rx.lang.scala.subjects.PublishSubject

import scala.language.reflectiveCalls
import scala.swing.Reactions.Reaction
import scala.swing.event.Event

/** Basic facilities for dealing with Swing-like components.
  *
  * Instead of committing to a particular widget implementation
  * functionality has been factored out here to deal only with
  * abstract types like `ValueChanged` or `TextField`.
  * Extractors for abstract events like `ValueChanged` have also
  * been factored out into corresponding abstract `val`s.
  */
trait SwingApi {

  type ValueChanged <: Event
  type ButtonClicked <: Event
  type TextField <: {
    def text: String
    def subscribe(r: Reaction): Unit
    def unsubscribe(r: Reaction): Unit
  }
  type Button <: {
    def subscribe(r: Reaction): Unit
    def unsubscribe(r: Reaction): Unit
  }
  val ValueChanged: {
    def unapply(x: Event): Option[TextField]
  }
  val ButtonClicked: {
    def unapply(x: Event): Option[Button]
  }

  implicit class TextFieldOps(field: TextField) {

    /** Returns a stream of text field values entered in the given text field.
      *
      * @return an observable with a stream of text field updates
      */
    def textValues: Observable[String] = {
      val subject = PublishSubject[String]("")
      field subscribe {
        case ValueChanged(f) => subject.onNext(field.text)
        case _ =>
      }
      subject
    }

  }

  implicit class ButtonOps(button: Button) {

    /** Returns a stream of button clicks.
      *
      * @return an observable with a stream of buttons that have been clicked
      */
    def clicks: Observable[Button] = {
      val subject = PublishSubject[Button](button)
      button subscribe {
        case ButtonClicked(b) => subject.onNext(b)
        case _ =>
      }
      subject
    }
  }
}