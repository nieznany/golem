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

        ((state ++ p) ++ getUpdatedFieldsForBreathRule(p, state)) ++ getDisabledFields(p, state)
      }
    }
  }

  def getUpdatedFieldsForBreathRule(put: Put, state: GameState): Set[Field] = {
    val besteadStone = put.stone
    val board = state.board

    val chains = (for {direction <- List(N, S, W, E)
          chainOption = getChain(besteadStone.position + direction, besteadStone.owner.opponent(), board)
          if chainOption != None
          chain = chainOption.asInstanceOf[Option[(Set[Field], Int)]].get
          if chain._2 == 0 // Only death chains
    } yield chain._1)
    if(! chains.isEmpty) {
      chains reduceLeft ((a, b) => a ++ b) map (stone => Free(stone.position))
    }
    else Set[Field]()
  }

  /**
   * @return as for the getChain(:Field, :Board), but return None, if coords points to stone,
   *         which does not belong to given player.
   */
  def getChain(memberCoords: Coords, player: Player, board: Board): Option[(Set[Field], Int)] = {
    val member = board(memberCoords)
    member match {
      case s: Stone => if (s.owner != player) None else getChain(member, board)
      case _ => getChain(member, board)
    }
  }

  def getChain(memberCoords: Coords, board: Board): Option[(Set[Field], Int)] = {
    val member = board(memberCoords)
    getChain(member, board)
  }

  /**
   *
   * @return (fields of chain, number of breaths) of chain starting in coords. Each field of chain
   *         belongs to owner of the stone placed at $memberCoords.
   *         Equals None, when given field is not a Stone.
   */
  def getChain(member: Field, board: Board): Option[(Set[Field], Int)] = {
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
      if (!visitedFields.contains(field)) {
        visitedFields += field
        field match {
          case s: Stone => {
            if (s.owner == player) {
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
