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

  case class NotEqualTo[A](a1: A, a2: A)(location: Location) extends RuntimeException{
    override def toString() = s"$a2 was not equal to $a1 ${simplifyLocation(location)}"
  }

}
