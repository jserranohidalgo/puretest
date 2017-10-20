package org.hablapps.puretest
package examples.tictactoe
package test

import cats.syntax.all._

trait TicTacToeFilterSpec[P[_]] extends FunSpec[P] with FilterSpec[P] {
  import TicTacToe._

  /* Evidence */

  val ticTacToe: TicTacToe[P]
  implicit val ME: cats.MonadError[P, PuretestError[Error]]

  /* Predicates */
  import ticTacToe.{ME => _, _}

  Describe("Reset Spec") {
    Holds("First turn is X") {
      reset >>
      currentTurnIs(X)
    }
  }

  Describe("Place Spec") {
    It("should not be possible to place more than one stone at the same place") {
      reset >>
      place(X, (1, 1)) >>
      place(O, (1, 1)).isError[Error](OccupiedPosition((1, 1)))
    }

    It("Placing outside of the board is error") {
      reset >>
      place(X, (5, 5)).isError[Error](NotInTheBoard((5, 5)))
    }

    It("Placing in the wrong turn") {
      reset >>
      place(O, (1, 1)).isError[Error](WrongTurn(O))
    }

    Holds("Turn must change") {
      (reset >>
        place(X, (1, 1)) >>
        currentTurnIs(O)) &&
      (place(O, (1, 2)) >>
        currentTurnIs(X))
    }

    Holds("Position must be occupied") {
      for {
        _ <- reset
        _ <- place(X, (1, 1))
        Some(X) <- in((1, 1))
      } yield true
    }
  }

  Describe("Win laws") {

    def winnerBoard: P[Unit] =
      reset >>
      place(X, (0, 0)) >>
      place(O, (1, 0)) >>
      place(X, (0, 1)) >>
      place(O, (2, 0)) >>
      place(X, (0, 2))

    Holds("Win at rows") {
      winnerBoard >>
      win(X)
    }

    Holds("No simultaneous winners") {
      winnerBoard >>
      win(O) map (! _)
    }

    Holds("No winner if not over") {
      reset >>
      winner map (! _.isDefined)
    }
  }

  Describe("Simulation behaviour") {
    It("Unfinished match") {
      reset >>
      simulate((0, 0), (0, 1))((1, 0), (1, 1)).isError[Error](NotEnoughMoves())
    }

    Holds("Finished match") {
      reset >>
      simulate((0, 0), (0, 1), (0, 2))((1, 0), (1, 1), (1, 2)) >>
      win(X)
    }
  }
}