package com.github.golem.army

import akka.event.{LoggingAdapter, Logging}
import com.github.golem.model.Board.{Stone, Free, FreeField}
import com.github.golem.model.{GameState, Put, BasicRulesGame, Board}
import com.github.golem.model.BasicRulesGame.Chain
import com.github.golem.model.GameState.MovesHistory


abstract class Private extends GolemActor {

  import context._

  val LOG = Logging(system, this)

  def getLogger: LoggingAdapter = LOG

  /**
   * choose move that will get for the chain the most liberties
   */
  def getBestMoveForChain(myChain: Chain, currentBoard: Board): Option[FreeField] = {
    val referenceStone = myChain.fields.head
    var bestMove: Option[FreeField] = None
    var currentMaxSize = myChain.breaths.size
    for (breath <- myChain.breaths) {
      val move = Put(Stone(breath.position, referenceStone.owner))
      val newGameState = BasicRulesGame.makeMove(move, GameState(new MovesHistory(), currentBoard))
      val newChain = game.getNonEmptyChain(referenceStone.position, newGameState.board)

      if (newChain.breaths.size > currentMaxSize) {
        bestMove = Some(breath)
        currentMaxSize = newChain.breaths.size
      }
    }
    bestMove
  }

  /**
   * choose move that will get for group the mose living fields
   * @param myChains chains of group
   * @param currentBoard
   * @return Pair of (best move, increase of living fields)
   */
  def getBestMoveForGroup(myChains: Set[Chain], currentBoard: Board): (Option[Free], Int) = {
    val referenceStone = myChains.head.fields.head
    val availableCoords = BasicRulesGame getGroupAvailablePositions(myChains, currentBoard)
    val currentGroupLives = BasicRulesGame getGroupLives(myChains, currentBoard)
    var bestIncrease = 0
    var bestMove: Option[Free] = None
    for (coord <- availableCoords) {
      val move = Put(Stone(coord, referenceStone.owner))
      val newGameState = BasicRulesGame.makeMove(move, GameState(new MovesHistory(), currentBoard))
      val newChains = newGameState.board.getDecomposedNonEmptyGroup(referenceStone.position).chains
      val increase: Int = BasicRulesGame.getGroupLives(newChains, newGameState.board) - currentGroupLives
      if (increase > bestIncrease) {
        bestIncrease = increase
        bestMove = Some(Free(coord))
      }
    }
    (bestMove, bestIncrease)
  }

}
