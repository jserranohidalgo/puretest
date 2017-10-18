package org.hablapps.puretest

import cats.MonadError

trait SimpleSpec[P[_], E] extends Filter.Syntax {

  implicit val ME: MonadError[P, E]
  implicit val Fi: Filter[P]

}
