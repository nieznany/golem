package com.github.golem.model

trait Move

case class Put(stone: Board.Stone) extends Move

case class Pass(player: Player) extends Move
