package org.hablapps.puretest
package scalatestImpl

trait ScalatestFunSpec extends org.scalatest.FunSpec with FunSpec with org.scalatest.Matchers{

  def Describe(subject: String)(test: => Unit): Unit =
    describe(subject)(test)

  import ProgramMatchers.Syntax._

  def It[P[_],A](condition: String)(
    program: => P[A])(implicit
    S: Tester[P,Throwable]): Unit =
    it(condition){
      program should runWithoutErrors
    }
}

