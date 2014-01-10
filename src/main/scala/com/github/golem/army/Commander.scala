package com.github.golem.army

import akka.actor.Actor
import com.github.golem.command.game.OpponentsMove

class Commander extends Actor {

  def receive: Actor.Receive = {
    case c: OpponentsMove =>
  }
}
