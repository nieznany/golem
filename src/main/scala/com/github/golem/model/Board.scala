package com.github.golem.model

import com.github.golem.model.Board.FieldState

object Board {
  object FieldState extends Enumeration {
    val FREE, DISABLED, OPPONENT, PLAYER = Value
  }
}

/**
 * Immutable.
 */
class Board(width: Int, height: Int) {
  private val fields = Array.fill[FieldState.Value](width, height) {FieldState.FREE}

  def +(move: Move) : Board = {
    move match {
      case _: Pass => this
      case m: Move => this + (move.position, if(move.opponent) FieldState.OPPONENT else FieldState.PLAYER)
    }
  }
  def apply(x: Int, y: Int): FieldState.Value = fields(x)(y)

  private def +(position: Move.Coordinates, state: FieldState.Value): Board = {
    // Assuming, that player made right move TODO this is not necessarily good...
    fields(position.x)(position.y) = state
    ???
  }

}
