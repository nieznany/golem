package com.github.golem.army

import akka.actor.Props
import com.github.golem.army.command.{Fun, Objective, SuggestMove}
import com.github.golem.model.{Pass, Put}
import com.github.golem.model.Board.Stone


object SpyLeader {
  def name = "spyleader"

  def props = Props(classOf[SpyLeader])
}

class SpyLeader extends Private {
  def handle(message: Any): Unit = {
    message match {
      case SuggestMove(gameState, privates, captains) => {
        val referenceStone = captains.getReferenceStoneFor(self)
        val currentBoard = gameState.board
        val myChain = currentBoard.getDecomposedNonEmptyGroup(referenceStone.position).chains.head

        val myMove = getBestMove(myChain, gameState.board) match {
          case Some(freeField) => Put(Stone(freeField.position, identity))
          case None => Pass(identity)
        }
        sender ! SuggestMove.Response(myMove, Fun())
      }
    }
  }
}
