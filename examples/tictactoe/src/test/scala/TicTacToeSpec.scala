package org.hablapps.puretest.examples.tictactoe
package test

import cats.implicits._
import org.hablapps.puretest._

trait TicTacToeSpec[P[_]] extends FunSpec[P] {

  /* Evidence */

  val ticTacToe: TicTacToe[P]

  /* Predicates */
  import ticTacToe._, TicTacToe._

  Describe("Reset Spec") {
    It("First turn X") {
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

    It("Turn must change") {
      (reset >>
        place(X, (1, 1)) >>
        currentTurnIs(O)) &&
      (place(O, (1, 2)) >>
        currentTurnIs(X))
    }

    It("Position must be occupied") {
      reset >>
      place(X, (1, 1)) >>
      in((1, 1)) map (_ contains X)
    }
  }

  Describe("Win laws") {

    def winnerBoard: P[Unit] =
      reset >>
      place(X,(0,0)) >>
      place(O,(1,0)) >>
      place(X,(0,1)) >>
      place(O,(2,0)) >>
      place(X,(0,2))

    It("Win at rows") {
      winnerBoard >>
      win(X)
    }

    It("No simultaneous winners") {
      winnerBoard >>
      win(O) map (! _)
    }

    It("No winner if not over") {
      reset >>
      winner map (! _.isDefined)
    }
  }

  Describe("Simulation behaviour") {
    It("Unfinished match") {
      reset >>
      simulate((0, 0), (0, 1))((1, 0), (1, 1)).isError[Error](NotEnoughMoves())
    }

    It("Finished match") {
      reset >>
      simulate((0, 0), (0, 1), (0, 2))((1, 0), (1, 1), (1, 2)) >>
      win(X)
    }
  }
}