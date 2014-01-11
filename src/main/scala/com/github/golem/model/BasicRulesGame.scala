package com.github.golem.model

import com.github.golem.exception.IllegalMoveException
import com.github.golem.model.Board._

// TODO how to avoid parameter 'state' in each method? Choose one of the possibilities
object BasicRulesGame extends Game {

  def makeMove(move: Move, state: GameState): GameState = {
    move match {
      case _: Pass => state + move
      case p: Put => {
        if (!isLegal(move, state))
          throw new IllegalMoveException(p)

        val updatedFields = Set[Field](p.stone) ++ getUpdatedFieldsForBreathRule(p, state) ++ getDisabledFields(move, state)
        state +(move, updatedFields)
      }
    }
  }

  def getUpdatedFieldsForBreathRule(put: Put, state: GameState): Set[Field] = {
    val besteadStone = put.stone
    val board = state.board

    val chains = for (direction <- List(N, S, W, E)) yield getChain(put.stone.position + direction, state.board)
    ???
  }

  /**
   *
   * @return (fields of chain, number of breaths)
   */
  def getChain(memberCoords: Coords, board: Board): Option[(Set[Field], Int)] = {
    val member = board(memberCoords)
    member match {
      case s: Stone => {
        val traverser = new ChainTraverser(board, s.owner)
        traverser.traverse(s)
        Some((traverser.chain, traverser.nbreaths))
      }
      case _ => None
    }
  }

  /**
   * Applies also ko rule.
   */
  def getDisabledFields(move: Move, state: GameState): Set[Field] = {
    ???
  }

  def isLegal(move: Move, state: GameState): Boolean = ???

  private class ChainTraverser(board: Board,
                                    player: Player,
                                    var chain: Set[Field] = Set[Field](),
                                    var nbreaths: Int = 0) {

    private var visitedFields = Set[Field]()

    def traverse(field: Field): Unit = {
      if(! visitedFields.contains(field)) {
        visitedFields += field
        field match {
          case s: Stone => {
            if(s.owner == player) {
              chain += s
              List(N, S, W, E) foreach (direction => traverse(board(field.position + direction)))
              traverse(s)
            }
          }
          case f: FreeField => {
            nbreaths += 1
          }
          case _: Field => 0 // NOP
        }
      }
    }
  }

}
