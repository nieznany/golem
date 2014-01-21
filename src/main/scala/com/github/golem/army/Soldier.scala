package com.github.golem.army

import akka.actor.Props
import com.github.golem.army.command.{Death, Defense, SuggestMove}
import com.github.golem.model.{Put, Pass}
import com.github.golem.model.Board.Stone
import scala.Some

object Soldier {
  def name = "soldier"

  def props = Props(classOf[Soldier])
}

class Soldier extends Private {

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
          case _ => Defense(myChain.fields.size, myChain.breaths.size)
        }

        sender ! SuggestMove.Response(myMove, objective)
      }
    }
  }
}
