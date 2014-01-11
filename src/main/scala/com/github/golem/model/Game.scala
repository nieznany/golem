package com.github.golem.model

trait Game {
  def isLegal(move: Move, state: GameState): Boolean

  def makeMove(move: Move, state: GameState): GameState
}

