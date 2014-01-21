package com.github.golem.army

import akka.actor.Props
import com.github.golem.army.command._
import com.github.golem.model.Put
import com.github.golem.model.Pass
import scala.Some
import com.github.golem.model.Board.Stone

object Captain {
  def name = "captain"

  def props = Props(classOf[Captain])
}

class Captain extends Private {

  def handle(message: Any): Unit = {
    message match {
      case SuggestMove(gameState, privates, captains) => {
        val referenceStone = captains.getReferenceStoneFor(self)
        val currentBoard = gameState.board
        // polecam teraz uzywac scachowane grupy/lancuch: patrz getDecomposedNonEmptyChain/Group
        val myChains = currentBoard.getDecomposedNonEmptyGroup(referenceStone.position).chains

        val (suggestedField,increase) = getBestMoveForGroup(myChains, currentBoard)
        val myMove = suggestedField match {
          case Some(freeField) => Put(Stone(freeField.position, identity))
          case None => Pass(identity)
        }
        sender ! SuggestMove.Response(myMove, DefendGroup(increase))
      }
    }
  }

}
