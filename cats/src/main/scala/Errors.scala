package org.hablapps.puretest

/**
 * Puretest errors
 */
trait Errors {

  case class NotEqualTo[A](a1: A, a2: A)(location: Location)
    extends RuntimeException(s"$a2 was not equal to $a1 ${simplifyLocation(location)}")

  case class NotFailed[A](value: A)(location: Location)
    extends RuntimeException(s"Error expected but found $value ${simplifyLocation(location)}")

  case class NotSucceeded[E](error: E)(location: Location)
    extends RuntimeException(s"Found error $error ${simplifyLocation(location)}")

  case class NotError[A, E](value: A, e: E)(location: Location)
    extends RuntimeException(s"$value was not equal to error $e ${simplifyLocation(location)}")

  case class OtherError[E](found: E, expected: E)(location: Location)
    extends RuntimeException(s"Error $found was not equal to error $expected ${simplifyLocation(location)}")

  case class FilterError(found: String)(location: Location)
    extends RuntimeException(s"$found doesn't match expected pattern ${simplifyLocation(location)}")

}
