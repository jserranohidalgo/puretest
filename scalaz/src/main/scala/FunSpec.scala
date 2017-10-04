package org.hablapps.puretest

trait FunSpec{

  def Describe(subject: String)(test: => Unit): Unit

  def It[P[_],A](condition: String)(
    program: => P[A])(implicit
    tester: Tester[P,Throwable]): Unit
}

