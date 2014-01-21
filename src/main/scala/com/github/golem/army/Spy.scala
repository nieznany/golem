package com.github.golem.army

import akka.actor.{Props, Actor}
import akka.event.{Logging, LoggingAdapter}
import com.github.golem.army.command._
import com.github.golem.model.Board.{Stone, Free}
import com.github.golem.model.{Pass, Put}
import com.github.golem.model.Put
import com.github.golem.army.command.Death
import com.github.golem.model.Pass
import com.github.golem.army.command.Attack
import scala.Some
import com.github.golem.model.Board.Stone

object Spy {
  def name = "spy"
  def props = Props(classOf[Spy])
}

class Spy extends Private {

  def handle(message: Any): Unit = {
    message match {
      case SuggestMove(gameState, privates, captains) => {
        val referenceStone = privates.getReferenceStoneFor(self)
        val currentBoard = gameState.board
        val myChain = game.getNonEmptyChain(referenceStone.position, currentBoard)

        val myMove = getBestMoveForChain(myChain, gameState.board) match {
          case Some(freeField) => Put(Stone(freeField.position, identity))
          case None => Pass(identity)
        }
        val objective = myChain.breaths.size match {
          case n if n == 1 => Death(myChain.fields.size)
          case _ => Attack(myChain.fields.size, myChain.breaths.size)
        }

        LOG.info("Spy suggests: "+ myMove + ", objective:" +objective)
        sender ! SuggestMove.Response(myMove, objective)
      }
    }
  }
}
