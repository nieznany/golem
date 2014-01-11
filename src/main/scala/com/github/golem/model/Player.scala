package com.github.golem.model

trait Player {
  def opponent(): Player
}

object Human extends Player {
  def opponent(): Player = Engine // Circular dependency
}

object Engine extends Player {
  def opponent(): Player = Human
}
