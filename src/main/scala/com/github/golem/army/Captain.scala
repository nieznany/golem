package com.github.golem.army

import akka.actor.Props
import com.github.golem.army.command._
import com.github.golem.model._
import com.github.golem.model.Put
import com.github.golem.model.Board.Free
import com.github.golem.model.GameState.MovesHistory
import com.github.golem.model.BasicRulesGame.Chain
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

        val myMove = bestMoveForGroup(myChains, currentBoard) match {
          case Some(freeField) => Put(Stone(freeField.position, identity))
          case None => Pass(identity)
        }
        sender ! SuggestMove.Response(myMove, Fun())
      }
    }
  }

  def bestMoveForGroup(myChains: Set[Chain], currentBoard: Board): Option[Free] = {
    val referenceStone = myChains.head.fields.head
    val availableCoords = BasicRulesGame getGroupAvailablePositions(myChains, currentBoard)

    val currentGroupLives: Int = BasicRulesGame getGroupLives(myChains, currentBoard)
    var bestIncrease = 0
    var bestMove: Option[Free] = None
    for (coord <- availableCoords) {
      println("considering coords:" + coord)
      val move = Put(Stone(coord, identity))
      val newGameState = BasicRulesGame.makeMove(move, GameState(new MovesHistory(), currentBoard))
      println(newGameState.board)
      val newChains = newGameState.board.getDecomposedNonEmptyGroup(referenceStone.position).chains
      println("newChains/" + newChains.size + ":" + newChains)
      val increase: Int = BasicRulesGame.getGroupLives(newChains, newGameState.board) - currentGroupLives
      if (increase > bestIncrease) {
        bestIncrease = increase
        bestMove = Some(Free(coord))
      }
      println("lives increase:" + increase)
    }
    bestMove
  }
}
