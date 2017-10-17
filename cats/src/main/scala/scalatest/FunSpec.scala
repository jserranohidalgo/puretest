package org.hablapps.puretest
package scalatestImpl

trait ScalatestFunSpec[P[_], E] extends org.scalatest.FunSpec
  with org.scalatest.Matchers
  with FunSpec[P] {

  implicit val Tester: Tester[P, E]

  def Describe(subject: String)(test: => Unit): Unit =
    describe(subject)(test)

  import ProgramMatchers.Syntax._

  def It(condition: String)(program: => P[Boolean]): Unit =
    it(condition) {
      program should beSatisfied
    }
}
