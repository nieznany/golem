package com.github.golem.army

import akka.event.{LoggingAdapter, Logging}
import com.github.golem.model.Board.FreeField
import com.github.golem.model.Board
import com.github.golem.model.BasicRulesGame.Chain


abstract class Private extends GolemActor {

  import context._

  val LOG = Logging(system, this)

  def getLogger: LoggingAdapter = LOG

  /**
   * choose move that will get for the chain the most liberties
   */
  def getBestMove(myChain: Chain, currentBoard: Board): Option[FreeField] = {
    val breathsCoords = (for (breath <- myChain.breaths) yield breath.position).toSet
    var currentMaxSize = breathsCoords.size
    var bestMove: Option[FreeField] = None
    for (breath <- myChain.breaths) {
      val neighbourFreeFields = game.getNeighbourFreeFields(breath.position, currentBoard)
      val newSize: Int = breathsCoords.size + neighbourFreeFields.size - 1
      if (newSize > currentMaxSize) {
        bestMove = Some(breath)
        currentMaxSize = newSize
      }
    }
    bestMove
  }

}
