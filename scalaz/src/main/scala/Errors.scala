package org.hablapps.puretest

import scalaz.MonadError

/**
 * Puretest errors
 */

sealed abstract class PureTestError[E](msg: String) extends RuntimeException(msg){
  override def toString = msg
}

object PureTestError {

  def simplifyLocation(location: Location): String = {
    val fileext = raw".*/(.*)".r
    val fileext(filename) = location._1.value
    s"($filename:${location._2.value})"
  }

  implicit def toPureTestError[E](e: E): PureTestError[E] =
    ApplicationError(e)

  implicit def fromPureTestError[E](e: PureTestError[E]): Option[E] =
    e match {
      case ApplicationError(e) => Some(e)
      case _ => None
    }

  implicit def toMonadError[P[_],E](implicit
    ME: MonadError[P,PureTestError[E]]) =
    new MonadError[P,E]{
      def point[A](a: => A) = ME.point(a)
      def bind[A,B](p: P[A])(f: A => P[B]) = ME.bind(p)(f)
      def raiseError[A](e: E) =
        ME.raiseError(toPureTestError(e))
      def handleError[A](p: P[A])(f: E => P[A]) =
        ME.handleError(p){ e2 => fromPureTestError(e2) match {
          case Some(e1) => f(e1)
          case None => ME.raiseError(e2)
        }}
    }
}

import PureTestError.simplifyLocation

case class ApplicationError[E](e: E) extends PureTestError[E](s"Application error: $e")

case class NotEqualTo[E,A](found: A, expected: A)(implicit location: Location)
  extends PureTestError[E](s"Value $expected expected but found value $found ${simplifyLocation(location)}")

case class NotFailed[E,A](found: A)(implicit location: Location)
  extends PureTestError[E](s"Error expected but found value $found ${simplifyLocation(location)}")

case class NotSucceeded[E](found: E)(implicit location: Location)
  extends PureTestError[E](s"Value expected but found error $found ${simplifyLocation(location)}")

case class NotError[A, E](found: A, expected: E)(implicit location: Location)
  extends PureTestError[E](s"Error $expected expected but found value $found ${simplifyLocation(location)}")

case class NotValue[A, E](found: E, expected: A)(implicit location: Location)
  extends PureTestError[E](s"Value $expected expected but found error $found ${simplifyLocation(location)}")

case class OtherError[E](found: E, expected: E)(implicit location: Location)
  extends PureTestError[E](s"Error $expected expected but found error $found ${simplifyLocation(location)}")

case class NotMatched[A,E](found: A)(implicit location: Location)
  extends PureTestError[E](s"Expected pattern doesn't match found value $found ${simplifyLocation(location)}")

case class NotMatchedFailure[E](found: E)(implicit location: Location)
  extends PureTestError[E](s"Expected pattern doesn't match found error $found ${simplifyLocation(location)}")

case class ShouldNotHappen[E](implicit location: Location)
  extends PureTestError[E](s"This error shouldn't ever be thrown ${simplifyLocation(location)}")