package org.hablapps.puretest

/**
 * Puretest errors
 */

sealed abstract class PureTestError[E](msg: String) extends RuntimeException(msg)

object PureTestError {
  import scalaz.{MonadError, ~>}

  def simplifyLocation(location: Location): String = {
    val fileext = raw".*/(.*)".r
    val fileext(filename) = location._1.value
    s"($filename:${location._2.value})"
  }


  implicit def toPureTestError[E](e: E): PureTestError[E] =
    ApplicationError(e)

  implicit def toMonadErrorApp[P[_],E](implicit ME: MonadError[P,PureTestError[E]]) =
    new MonadError[P,E]{
      def point[A](a: => A) = ME.point(a)
      def bind[A,B](p: P[A])(f: A => P[B]) = ME.bind(p)(f)
      def raiseError[A](e: E) =
        ME.raiseError(ApplicationError(e))
      def handleError[A](p: P[A])(f: E => P[A]) =
        ME.handleError(p){
          case ApplicationError(e) => f(e)
          case _ => p
        }
    }
}

import PureTestError.simplifyLocation

case class ApplicationError[E](e: E) extends PureTestError[E](s"Application error: $e")

case class NotEqualTo[E,A](found: A, expected: A)(location: Location)
  extends PureTestError[E](s"Value $expected expected but found value $found ${simplifyLocation(location)}")

case class NotFailed[E,A](found: A)(location: Location)
  extends PureTestError[E](s"Error expected but found value $found ${simplifyLocation(location)}")

case class NotSucceeded[E](found: E)(location: Location)
  extends PureTestError[E](s"Value expected but found error $found ${simplifyLocation(location)}")

case class NotError[A, E](found: A, expected: E)(location: Location)
  extends PureTestError[E](s"Error $expected expected but found value $found ${simplifyLocation(location)}")

case class NotValue[A, E](found: E, expected: A)(location: Location)
  extends PureTestError[E](s"Value $expected expected but found error $found ${simplifyLocation(location)}")

case class OtherError[E](found: E, expected: E)(location: Location)
  extends PureTestError[E](s"Error $expected expected but found error $found ${simplifyLocation(location)}")

case class FilterError[E](found: String)(location: Location)
  extends PureTestError[E](s"$found doesn't match expected pattern ${simplifyLocation(location)}")

case class ShouldNotHappen[E](location: Location)
  extends PureTestError[E](s"This error shouldn't be thrown ${simplifyLocation(location)}")