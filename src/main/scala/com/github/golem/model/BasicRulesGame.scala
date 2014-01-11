package com.github.golem.model

object BasicRulesGame extends Game {

  def makeMove(move: Move, state: GameState): GameState = {
    move match {
      case _: Pass => state
      case p: Put => {
        state
      }
    }
  }

  def isLegal(move: Move, state: GameState): Boolean = ???
}
