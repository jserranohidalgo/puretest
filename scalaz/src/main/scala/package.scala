package org.hablapps

package object puretest
  extends StateTMonadError
  with StateTEqual
  with StateTArbitrary
  with StateValidationMonad{

  type Location = (sourcecode.File, sourcecode.Line)

  implicit def loc(implicit f: sourcecode.File, l: sourcecode.Line) = (f,l)

  /* matchers and ops */

  implicit def toPureMatchers[P[_],A](p: P[A]) =
    new PureMatchers(p)

  implicit def toBooleanOps[P[_]](p: P[Boolean]) =
    new BooleanOps(p)

}
