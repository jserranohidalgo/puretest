package org.hablapps.puretest
package examples.tictactoe
package test

import scalatestImpl.ScalatestFunSpec
import cats.{MonadError, ~>}
import cats.instances.either._

import Implicits._
import BoardStateSpec._

class BoardStateSpec extends ScalatestFunSpec[Program, TicTacToe.Error]
    with TicTacToeSpec[Program] { self =>

  lazy val ticTacToe = new TicTacToe[Program] {

    private val nat = implicitly[BoardState.Program ~> Program]

    val ME = MonadError[Program, TicTacToe.Error]

    def reset: Program[Unit] =
      nat(BoardState.BoardTicTacToe.reset)
    def place(stone: TicTacToe.Stone, position: TicTacToe.Position): Program[Unit] =
      nat(BoardState.BoardTicTacToe.place(stone, position))

    def win(stone: TicTacToe.Stone): Program[Boolean] =
      nat(BoardState.BoardTicTacToe.win(stone))
    def in(position: TicTacToe.Position): Program[Option[TicTacToe.Stone]] =
      nat(BoardState.BoardTicTacToe.in(position))
    def turn: Program[Option[TicTacToe.Stone]] =
      nat(BoardState.BoardTicTacToe.turn)
  }
  lazy val Tester = StateTester[Program, BoardState, PuretestError[TicTacToe.Error]].apply(BoardState.empty)
  implicit val ME = MonadError[Program, PuretestError[TicTacToe.Error]]

}

object BoardStateSpec {
  import cats.data.StateT

  type Program[T] = StateT[Either[PuretestError[TicTacToe.Error], ?], BoardState, T]
}
