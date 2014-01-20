package com.github.golem.army

import akka.actor.Props
import com.github.golem.army.command.{Objective, SuggestMove}
import com.github.golem.model.{Pass, Put}
import com.github.golem.model.Board.Stone

object Captain {
  def name = "captain"

  def props = Props(classOf[Captain])
}

class Captain extends Private {

  def handle(message: Any): Unit = {
    message match {
      case SuggestMove(gameState, siblings) => {
        val referenceStone = siblings.getReferenceStoneFor(self)
        val currentBoard = gameState.board
        // polecam teraz uzywac scachowane grupy/lancuch: patrz getDecomposedNonEmptyChain/Group
        val myChain = currentBoard.getDecomposedNonEmptyGroup(referenceStone.position).chains.head

        val myMove = getBestMove(myChain, gameState.board) match {
          case Some(freeField) => Put(Stone(freeField.position, identity))
          case None => Pass(identity)
        }
        sender ! SuggestMove.Response(myMove, Objective(Some(myChain)))
      }
    }
  }
}
