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

case class NotEqualTo[E,A](a1: A, a2: A)(location: Location)
  extends PureTestError[E]("$a2 was not equal to $a1 ${simplifyLocation(location)}")

case class NotFailed[E,A](value: A)(location: Location)
  extends PureTestError[E]("Error expected but found $value ${simplifyLocation(location)}")

case class NotSucceeded[E](error: E)(location: Location)
  extends PureTestError[E]("Found error $error ${simplifyLocation(location)}")

case class NotError[A, E](value: A, e: E)(location: Location)
  extends PureTestError[E]("$value was not equal to error $e ${simplifyLocation(location)}")

case class OtherError[E](found: E, expected: E)(location: Location)
  extends PureTestError[E]("Error $found was not equal to error $expected ${simplifyLocation(location)}")

case class FilterError[E](found: String)(location: Location)
  extends PureTestError[E]("$found doesn't match expected pattern ${simplifyLocation(location)}")

