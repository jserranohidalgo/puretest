package org.hablapps.puretest

/**
 * Puretest errors
 */
trait Errors {

  sealed trait PuretestError[E]
  case class ApplicationError[E](e: E) extends PuretestError[E]
  case class TestingErrorW[E](e: TestingError) extends PuretestError[E] {
    override def toString() = e.getMessage
  }

  object PuretestError {
    import cats.~>

    implicit def eitherToPuretestError[E] =
      λ[Either[E, ?] ~> Either[PuretestError[E], ?]] {
        case Left(e) => Left(ApplicationError(e))
        case Right(a) => Right(a)
      }

  }

  sealed trait TestingError extends RuntimeException

  case class NotEqualTo[A](a1: A, a2: A)(location: Location)
    extends RuntimeException(s"$a2 was not equal to $a1 ${simplifyLocation(location)}")
    with TestingError

  case class NotFailed[A](value: A)(location: Location)
    extends RuntimeException(s"Error expected but found $value ${simplifyLocation(location)}")
    with TestingError

  case class NotSucceeded[E](error: E)(location: Location)
    extends RuntimeException(s"Found error $error ${simplifyLocation(location)}")
    with TestingError

  case class NotError[A, E](value: A, e: E)(location: Location)
    extends RuntimeException(s"$value was not equal to error $e ${simplifyLocation(location)}")
    with TestingError

  case class OtherError[E](found: E, expected: E)(location: Location)
    extends RuntimeException(s"Error $found was not equal to error $expected ${simplifyLocation(location)}")
    with TestingError

  case class FilterError(found: String)(location: Location)
    extends RuntimeException(s"$found doesn't match expected pattern ${simplifyLocation(location)}")
    with TestingError

}
