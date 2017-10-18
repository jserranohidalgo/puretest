package org.hablapps.puretest

/**
 * Puretest errors
 */
trait Errors {

  case class NotError[A](value: A, e: Throwable)(location: Location) extends RuntimeException {
    override def toString() = s"$value was not equal to error $e ${simplifyLocation(location)}"
  }

  case class OtherError(error: Throwable, e: Throwable)(location: Location) extends RuntimeException {
    override def toString() = s"Error $error was not equal to error $e ${simplifyLocation(location)}"
  }

  case class NotEqualTo[A](a1: A, a2: A)(location: Location) extends RuntimeException {
    override def toString() = s"$a2 was not equal to $a1 ${simplifyLocation(location)}"
  }

  // new

  case class OtherError2[E](found: E, expected: E)(location: Location)
    extends RuntimeException(s"Error $found was not equal to error $expected ${simplifyLocation(location)}")

  case class NotError2[A, E](value: A, e: E)(location: Location)
    extends RuntimeException(s"$value was not equal to error $e ${simplifyLocation(location)}")

  case class FilterError(found: String)(location: Location)
    extends RuntimeException(s"$found doesn't match expected pattern ${simplifyLocation(location)}")

}
