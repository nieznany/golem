package com.github.golem.army

import akka.actor.{Props, Actor}
import com.github.golem.model.{Pass, Engine, GameState}
import com.github.golem.command.game.{GenerateMove, MadeMove}
import com.github.golem.command.Informative
import com.github.golem.command.Command
import akka.event.Logging

object Commander {
  def props = Props(classOf[Commander])
}

class Commander extends Actor {
  import context._
  /** Identity of commander */
  val identity = Engine
  private val LOG = Logging(system, this)
  private val currentGameState: Option[GameState] = None

  def receive: Actor.Receive = {
    case m: Informative => {

    }
    case m: Command => {
      m match {
        case GenerateMove => {
          sender ! GenerateMove.Response(Pass(identity)) // TODO change that!
        }
      }
    }
  }

}
