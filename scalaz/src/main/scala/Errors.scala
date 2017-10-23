package org.hablapps.puretest

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