package com.github.golem.army

import akka.actor.Props
import com.github.golem.army.command.{Defense, SuggestMove}
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

        sender ! SuggestMove.Response(myMove, Defense(myChain.fields.size, myChain.breaths.size))
      }
    }
  }
}
