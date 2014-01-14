package com.github.golem.model

trait Player {
  def opponent(): Player
}

object Human extends Player {
  def opponent(): Player = Engine // Circular dependency
  override def toString = this.getClass.getSimpleName
}

object Engine extends Player {
  def opponent(): Player = Human
  override def toString = this.getClass.getSimpleName
}
