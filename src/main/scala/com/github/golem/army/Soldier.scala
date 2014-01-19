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

class Soldier extends GolemActor {

  import context._

  val LOG = Logging(system, this)

  def getLogger: LoggingAdapter = LOG

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

  /**
   * choose move that will get for the chain the most liberties 
   */
  def getBestMove(myChain: Chain, currentBoard: Board): Option[FreeField] = {
    val breathsCoords = (for (breath <- myChain.breaths) yield breath.position).toSet
    var currentMaxSize = breathsCoords.size
    var bestMove: Option[FreeField] = None
    LOG.info("suggesting move, current breaths:" + currentMaxSize)
    for (breath <- myChain.breaths) {
      val neighbourFreeFields = game.getNeighbourFreeFields(breath.position, currentBoard)
      val newSize: Int = breathsCoords.size + neighbourFreeFields.size - 1
      LOG.info("considered size:" + newSize)
      if (newSize > currentMaxSize) {
        bestMove = Some(breath)
        currentMaxSize = newSize
      }
    }
    bestMove
  }
}
