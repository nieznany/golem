package com.github.golem.army

import akka.actor.{Props, Actor}
import com.github.golem.command.game.MadeMove
import com.github.golem.model.GameState

object Commander {
  def props = Props(classOf[Commander])
}

class Commander extends Actor {
  val currentGameState: Option[GameState] = None

  def receive: Actor.Receive = {
  }
}
