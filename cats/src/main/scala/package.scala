package org.hablapps

package object puretest
  extends StateValidatedMonad
  with TestingOps
  with Errors {

  type Location = (sourcecode.File, sourcecode.Line)

  def simplifyLocation(location: Location): String = {
    val fileext = raw".*/(.*)".r
    val fileext(filename) = location._1.value
    s"($filename:${location._2.value})"
  }

  import cats.{Applicative, FlatMap, ~>}
  import cats.data.StateT

  implicit def stateTTransformer[F[_]: FlatMap, G[_]: Applicative, S](implicit nat: F ~> G) =
    new (StateT[F, S, ?] ~> StateT[G, S, ?]) {
      def apply[A](from: StateT[F, S, A]) = StateT[G, S, A] { s =>
        val fsa = from.run(s)
        nat(fsa)
      }
    }

}