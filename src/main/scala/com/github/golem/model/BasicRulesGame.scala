package com.github.golem.model

import com.github.golem.exception.IllegalMoveException
import com.github.golem.model.Board._

// TODO how to avoid parameter 'state' in each method? Choose one of the possibilities
object BasicRulesGame extends Game {

  case class Chain(fields: Set[Stone], breaths: Set[FreeField])

  val DIRECTIONS = List(N, S, W, E)

  def makeMove(move: Move, state: GameState): GameState = {
    move match {
      case _: Pass => state + move
      case p: Put => {
        if (!isLegal(move, state))
          throw new IllegalMoveException(p)

        val opponent = p.stone.owner.opponent() // We are making game state for this player.

        val newFreeFields = getEndangeredStones(p, state.board) map (stone => stone.toFree)

        val state1 = ((state ++ p) ++ newFreeFields) // Breath rule
        val state2 = state1 ++ updateAvailabilityOfFields(opponent, state1.board) // Add (very useful) information, which fields are unavailable for player.

        // Ko rule.
        if (newFreeFields.size == 1) {
          // Only one stone has removed => there might possible reverse move
          val suspiciousField = newFreeFields.head
          // Let's check, which fields will be endangered, when i next move stone will be placed at suspicious field.
          val nextEndangeredFields = getEndangeredStones(new Put(newFreeFields.head, opponent), state2.board)
          if (nextEndangeredFields.size == 1 && nextEndangeredFields.contains(p.stone)) {
            // In next move there will be possibility to reverse back board () suspicious Field should be unavailable
            return state2 + new Unavailable(suspiciousField, opponent)
          }
        }
        state2
      }
    }
  }

  /**
   * @return
   */
  def getEndangeredStones(put: Put, board: Board): Set[Stone] = {
    val deathFieldSets = for (c <- getEndangeredChains(put, board)) yield c.fields

    if (!deathFieldSets.isEmpty) {
      deathFieldSets reduceLeft ((a, b) => a ++ b)
    }
    else Set[Stone]()
  }

  /**
   * Finds all chains which belongs to opponent (of player, who will make given move), which will be dead, if player
   * will do that move. So for example, you can find, which chains of human player will be dead after
   * move made by engine player.
   *
   * @param put - action, which was performed by player.
   * @return all of chains, which should be removed, if player made move $put on provided board.
   *         If selected field is disabled (i.e. is occupied by some stone), none of neighbour chain is endangered,
   *         so empty set is returned.
   */
  def getEndangeredChains(put: Put, board: Board): Set[Chain] = {
    val besteadStone = put.stone
    val chains = scala.collection.mutable.Set[Chain]()
    if (board.isDisabled(besteadStone.position)) return chains.toSet

    for (direction <- DIRECTIONS) {
      val field = board(besteadStone.position + direction)
      field match {
        case stone: Stone => {
          if (chains.filter(chain => chain.fields.contains(stone)).isEmpty) {
            // field does not belong to one of found chains
            val result = getChain(stone, besteadStone.owner.opponent(), board)
            result match {
              case Some(chain) => {
                if (!chain.fields.isEmpty
                  && (chain.breaths.size == 0 // impossible
                  || (chain.breaths.size == 1 && chain.breaths.contains(besteadStone.toFree)))) // the only breath will be taken by bestead stone
                  chains += chain
              }
              case None => {}
            }
          }
        }
        case _ => {}
      }
    }
    chains.toSet
  }

  def getChain(memberCoords: Coords, board: Board): Option[Chain] = {
    val member = board(memberCoords)
    getChain(member, board)
  }

  @throws[IllegalArgumentException]("If selected field is not a stone.")
  def getNonEmptyChain(memberCoords: Coords, board: Board): Chain = {
    getChain(memberCoords, board) match {
      case Some(chain) => chain
      case None => throw new IllegalArgumentException(s"$memberCoords, does not point to any chain at $board")
    }
  }

  def isGhostChain(chain: Chain, board: Board): Boolean = {
    chain.fields foreach {
      field =>
        board(field.position) match {
          case _: Stone => return false
          case _ => {}
        }
    }
    true
  }

  def getNeighbourStones(coords: Coords, player: Player, board: Board): Set[Stone] = {
    getNeighbourFields(coords, board) filter {
      case Stone(_, owner) => owner == player
      case _ => false
    } map {
      field => field.asInstanceOf[Stone]
    }
  }

  def getChainNeighbourFields(chain: Chain, player: Player, board: Board): Set[Coords] = {
    val neighbours = scala.collection.mutable.Set[Coords]()
    for (stone <- chain.fields) {
      val neighbourFreeFields = getRegionNeighbourFields(stone.position, player, board)
      neighbourFreeFields foreach (field => neighbours.add(field.position))
    }
    return neighbours.toSet
  }

  def getRegionNeighbourFields(coords: Coords, player: Player, board: Board): Set[Field] = {
    getNeighbourFields(coords, board) filter {
      case Stone(_, owner) => owner != player
      case Free(_) => true
      case _ => false
    }
  }

  def getNeighbourFields(coords: Coords, board: Board): Set[Field] = {
    (for (d <- DIRECTIONS) yield board(coords + d)).toSet
  }

  def getNeighbourFreeFields(coords: Coords, board: Board): Set[Field] = {
    getNeighbourFields(coords, board) filter (field => field.isInstanceOf[FreeField])
  }

  def getNeighbourNonFreeFields(coords: Coords, board: Board): Set[Field] = {
    getNeighbourFields(coords, board) filter (field => !field.isInstanceOf[FreeField])
    // or neighbour fields - neighbour non free fields, but now is quite more efficient
  }

  def isLegal(move: Move, state: GameState): Boolean = {
    move match {
      case Put(Stone(Coords(r, c), owner)) =>
        if (state.board.isOutOfBounds(r, c))
          return false

        val field = state.board(r, c)
        field match {
          case Unavailable(_, `owner`) => false
          case ff: FreeField => true
          case _ => false
        }
      case _ => true
    }
  }

  /**
   * Note: in most cases, you should not use this method - you can
   * find information about unavailable fields board directly.
   *
   * @return true, if given player cannot put stone on a given field.
   */
  def isPlayerBlocked(player: Player, coords: Coords, board: Board): Boolean = {
    val fakeMove = Put(Stone(coords, player))
    // Check, if after this move, new stone of player will have breathe
    val endangeredOpponentChains = getEndangeredChains(fakeMove, board)
    if (!endangeredOpponentChains.isEmpty) return false // Yes!
    // If no, check, whether this is suicide move
    val playerChain = getChain(coords, board + fakeMove.stone) // TODO very inefficient, copying whole board!
    playerChain match {
      case Some(chain) => {
        chain.breaths.isEmpty
      }
      case None => throw new RuntimeException("Impossible happens!")
    }
  }

  /**
   * @return the as for the getChain(:Field, :Board), but returns None, if coords points to stone,
   *         which does not belong to given player.
   */
  private def getChain(member: Field, player: Player, board: Board): Option[Chain] = {
    member match {
      case s: Stone => if (s.owner != player) None else getChain(member, board)
      case _ => getChain(member, board)
    }
  }

  /**
   * <b>Should be used carefully!</b>
   *
   * @param member - member and approriate field on board might be different (you can test, what chains appears for
   *               fictional field). TODO change to coords
   * @return (fields of chain, number of breaths) of chain starting in coords. Each field of chain
   *         belongs to owner of the stone placed at $memberCoords.
   *         Equals None, when given field is not a Stone.
   */
  // TODO should be optimized, i.e store chains in GameState object
  private def getChain(member: Field, board: Board): Option[Chain] = {
    member match {
      case s: Stone => {
        val traverser = new ChainTraverser(board, s.owner)
        traverser.traverse(s)
        traverser.getChain
      }
      case _ => None
    }
  }

  /**
   * @return fields, which are not available for $currentPlayer (if currently he makes move).
   *         TODO maybe should include fields, which will be not avaialble in next move?
   */
  def updateAvailabilityOfFields(currentPlayer: Player, board: Board): Set[FreeField] = {
    val fields = scala.collection.mutable.Set[FreeField]()
    for (r <- 1 to board.nrows) {
      for (c <- 1 to board.ncolumns) {
        val coords = Coords(r, c)
        val currentField = board(coords)
        currentField match {
          case f: FreeField => {
            if (isPlayerBlocked(currentPlayer, coords, board)) {
              fields += new Unavailable(f, currentPlayer)
            }
            else {
              fields += new Free(f.position)
            }
          }
          case _ => {}
        }
      }
    }
    fields.toSet
  }

  /**
   * Checks if given group of chains is unconditionally alive. Based on Benson's algorithm.
   * @param chains set of chains forming 1 group
   * @param board current board
   * @return true if all chains are alive, otherwise false
   */
  def isGroupAlive(chains: Set[Chain], board: Board): Boolean = {
    var regionsMap = scala.collection.mutable.Map[Int, Set[Coords]]()
    var chainsMap = scala.collection.mutable.Map[Int, Chain]()
    var chainsToRegionsMap = scala.collection.mutable.Map[Int, scala.collection.mutable.Set[Int]]()
    var regionsToChainsMap = scala.collection.mutable.Map[Int, scala.collection.mutable.Set[Int]]()


    chains foreach (chain => chainsMap += chainsMap.size -> chain)

    if (chains isEmpty) {
      throw new Exception("Trying to evaluate group with no chains")
    }
    val player = chains.head.fields.head.owner
    for ((chainKey, chain) <- chainsMap) {
      val neighbourCoords = getChainNeighbourFields(chain, player, board)
      for (coord <- neighbourCoords) {
        var found: Boolean = false
        for ((regionKey, region) <- regionsMap) {
          if (region.contains(coord)) {
            found = true
            regionsToChainsMap.get(regionKey) match {
              case None => regionsToChainsMap += regionKey -> scala.collection.mutable.Set(chainKey)
              case Some(set) => set += chainKey
            }
            if (region.subsetOf(neighbourCoords)) {
              chainsToRegionsMap.get(chainKey) match {
                case None => chainsToRegionsMap += chainKey -> scala.collection.mutable.Set(regionKey)
                case Some(set) => set += regionKey
              }
            }
          }
        }
        if (!found) {
          val traverser = new RegionTraverser(board, player)
          traverser.growRegion(coord)
          val regionId = regionsMap.size
          regionsMap += regionId -> traverser.region
          regionsToChainsMap += regionId -> scala.collection.mutable.Set(chainKey)
          if (traverser.region.subsetOf(neighbourCoords)) {
            chainsToRegionsMap.get(chainKey) match {
              case None => chainsToRegionsMap += chainKey -> scala.collection.mutable.Set(regionId)
              case Some(set) => set += regionId
            }
          }
        }
      }
    }

    var compromisedRegions = scala.collection.mutable.Set[Int]()
    var compromisedChains = scala.collection.mutable.Set[Int]()
    var sthChanged = false
    do {
      sthChanged = false
      for ((key, chain) <- chainsMap) {
        chainsToRegionsMap.get(key) match {
          case Some(set) =>
            compromisedRegions foreach (regionId => set.remove(regionId))
            if (set.size < 2) {
              compromisedChains += key
              chainsToRegionsMap.remove(key)
              sthChanged = true;
            }
          case None =>
        }
      }
      for ((key, region) <- regionsMap) {
        regionsToChainsMap.get(key) match {
          case Some(set) => set foreach (chainId => if (compromisedChains.contains(chainId)) {
            sthChanged = true
            compromisedRegions += key
            set.remove(chainId)
          })
          case None =>
        }
      }
    } while (sthChanged)

    compromisedChains.isEmpty
  }

  private class ChainTraverser(board: Board,
                               player: Player,
                               var chainFields: Set[Stone] = Set[Stone](),
                               var breaths: Set[FreeField] = Set[FreeField]()) {

    private var visitedFields = Set[Field]()

    /**
     * TODO change this method, to use only coords as a parameters!
     * <b> Note - should be used with carefull! <b/>
     * @param field - <b> may be not the same, as field on board - i.e. allows to check,
     *              what chains appears for fictional field.</b>
     */
    def traverse(field: Field): Unit = {
      if (!visitedFields.contains(field)) {
        visitedFields += field
        field match {
          case s: Stone => {
            if (s.owner == player) {
              chainFields += s
              DIRECTIONS foreach (direction => traverse(board(field.position + direction)))
              traverse(s)
            }
          }
          case f: FreeField => {
            breaths += f
          }
          case _: Field => 0 // NOP
        }
      }
    }

    def getChain = if (chainFields.isEmpty) None else Some(Chain(chainFields, breaths))
  }

  private class RegionTraverser(board: Board, player: Player, var region: Set[Coords] = Set[Coords]()) {

    def growRegion(coords: Coords): Unit = {
      region += coords
      for (field <- getRegionNeighbourFields(coords, player, board)) {
        if (!region.contains(field.position)) {
          region += field.position
          growRegion(field.position)
        }
      }
    }
  }

}
