package com.github.golem.model

import com.github.golem.model.Board.Field
import com.github.golem.model.GameState.MovesHistory

object GameState {
  case class MovesHistory(moves: List[Move]) {
    def +(move: Move): MovesHistory = copy(moves = move :: moves)

    /**
     * The latest = apply(0)
     * The oldest = apply(n)
     **/
    def apply(timeFromNow: Int): Move = moves(timeFromNow)
  }
}

case class GameState(history: MovesHistory, board: Board) {
  /**
   * Updates only list of moves.
   *
   * @param move move to add
   * @return new game state with updated list of moves.
   */
  def +(move: Move): GameState = copy(history = this.history + move)

  def ++(move: Move): GameState = {
    move match {
      case pass: Pass => this + pass
      case put: Put => copy(history = this.history + move, board = this.board + put.stone)
    }
  }

  def ++(fields: Iterable[Field]): GameState = copy(board = this.board ++ fields)

  def ++(move: Move, fields: Iterable[Field]): GameState = copy(history = this.history + move, board = this.board ++ fields)

  def getMove(i: Int): Move = history(i)

  def getLastMove: Move = getMove(0)
}

