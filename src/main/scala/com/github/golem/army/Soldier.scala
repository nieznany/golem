package com.github.golem.army

import akka.actor.Props
import akka.event.{LoggingAdapter, Logging}
import com.github.golem.army.command.{Objective, SuggestMove}
import com.github.golem.model.{Board, Put, Pass}
import com.github.golem.model.Board.{FreeField, Stone}
import scala.Some
import com.github.golem.model.BasicRulesGame.Chain

object Soldier {
  def name = "soldier"

  def props = Props(classOf[Soldier])
}

class Soldier extends Private {

  def handle(message: Any): Unit = {
    message match {
      case SuggestMove(gameState, siblings) => {
        val referenceStone = siblings.getReferenceStoneFor(self)
        val currentBoard = gameState.board
        val myChain = game.getNonEmptyChain(referenceStone.position, currentBoard)

        val myMove = getBestMove(myChain, gameState.board) match {
          case Some(freeField) => Put(Stone(freeField.position, identity))
          case None => Pass(identity)
        }
        sender ! SuggestMove.Response(myMove, Objective(Some(myChain)))
      }
    }
  }
}
