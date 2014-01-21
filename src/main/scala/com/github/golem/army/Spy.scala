package com.github.golem.army

import akka.actor.{Props, Actor}
import akka.event.{Logging, LoggingAdapter}
import com.github.golem.army.command.{Attack, Objective, SuggestMove}
import com.github.golem.model.Board.{Stone, Free}
import com.github.golem.model.{Pass, Put}

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
        sender ! SuggestMove.Response(myMove, Attack(myChain.fields.size, myChain.breaths.size))
      }
    }
  }
}
