package com.github.golem.model

import com.github.golem.model.Board.{Stone, FreeField}

trait Move

case class Put(stone: Board.Stone) extends Move {
  def this(freeField: FreeField, player: Player) = {
    this(Stone(freeField.position, player))
  }
}

case class Pass(player: Player) extends Move

