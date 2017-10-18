package org.hablapps.puretest
package examples.tictactoe
package test

import scalatestImpl.ScalatestFunSpec
import cats.instances.either._

class BoardStateFilterSpec extends ScalatestFunSpec[BoardState.Program, TicTacToe.Error]
    with TicTacToeFilterSpec[BoardState.Program] {

  val ticTacToe = BoardState.BoardTicTacToe
  val Tester = StateTester[BoardState.Program, BoardState, TicTacToe.Error].apply(BoardState.empty)
  val Fi = Filter[BoardState.Program]

}
