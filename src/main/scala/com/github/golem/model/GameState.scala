package com.github.golem.model

object GameState {
  /**
   * Creates empty list of moves.
   * @return
   */
  def apply() = GameState(List())
}

case class GameState(moves: List[Move]) {

}
