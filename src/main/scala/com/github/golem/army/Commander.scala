package com.github.golem.army

import akka.actor.{PoisonPill, ActorRef, Props, Actor}
import com.github.golem.model._
import com.github.golem.command.game.{GenerateMove, MadeMove}
import com.github.golem.command.Informative
import com.github.golem.command.Command
import akka.event.{LoggingAdapter, Logging}
import com.github.golem.command.setup.{SetKomi, QuitGame, SetBoardSize}
import com.github.golem.model.Pass
import scala.Some
import com.github.golem.model.Board.{FreeField, Stone, Coords}
import com.github.golem.model.BasicRulesGame.Chain
import com.github.golem.army.Commander.Subordinates

object Commander {
  def props = Props(classOf[Commander])

  /**
   * Bidirectional map for subordinate <-> one of positions (stones) on board
   */
  case class Subordinates(private val referenceStones: Map[ActorRef, Stone] = Map[ActorRef, Stone](),
                          private val subordinatesMap: Map[Coords, ActorRef] = Map[Coords, ActorRef]()) {

    def getActors: Set[ActorRef] = referenceStones.keySet

    def getReferenceStoneFor(actor: ActorRef): Stone = referenceStones(actor)

    def getSubordinateFor(coords: Coords): Option[ActorRef] = subordinatesMap.get(coords)

    def +(acs: Iterable[(ActorRef, Chain)]): Subordinates = {
      var newRefPositions = referenceStones
      var newSubMap = subordinatesMap
      for (ac <- acs) {
        newRefPositions += (ac._1 -> ac._2.fields.head)
        newSubMap ++= (for (stone <- ac._2.fields) yield (stone.position -> ac._1))
      }
      Subordinates(newRefPositions, newSubMap)
    }

    def -(actors: Set[ActorRef]): Subordinates = {
      var newSubMap = subordinatesMap
      var newRefPositions = referenceStones
        for (kv <- subordinatesMap) {
          // FIX very inefficient, but most general (it will actually remove all actor's stones) - what to do with this method?
          if(actors.contains(kv._2))
            newSubMap -= kv._1
        }
      for(actor <- actors)
        newRefPositions -= actor
      Subordinates(newRefPositions, newSubMap)
    }

  }

}

class Commander extends GolemActor {

  import context._

  /** Identity of commander */
  val identity: Player = Engine
  private val LOG = Logging(system, this)
  private val game = BasicRulesGame
  // IDEA: handle other types of rules
  private var currentGameState: Option[GameState] = None
  private var subordinates: Subordinates = new Subordinates

  def handle(message: Any) = {
    message match {
      case i: Informative => {
        i match {
          case SetBoardSize(size) => {
            // Start new game
            currentGameState = Some(new GameState(Board(size, size)))
            subordinates = Subordinates()
            LOG.info(s"Starting new game: $currentGameState")
          }
          case SetKomi(_) => {
            LOG.info("Ignoring information about Komi.")
          }

          case QuitGame => {
            LOG.info(s"Finishing game, result state: $currentGameState")
            subordinates.getActors foreach (subordinate => subordinate ! PoisonPill)
            currentGameState = None
            subordinates = Subordinates()
          }
          case MadeMove(move) => {
            updateFor(move)
            LOG.info(s"New game state: $currentGameState") // TODO change to debug
            LOG.info(s"$subordinates")
          }
        }
      }
      case cmd: Command => {
        cmd match {
          case GenerateMove => {
            // TODO - do something
            sender ! GenerateMove.Response(Pass(identity))
          }
        }
      }
      case ufo => {
        LOG.warning(s"I saw UFO: $ufo. I will ignore it...")
      }
    }
  }

  /**
   * Updates actor's state for given move.
   */
  private def updateFor(move: Move): Unit = {
    currentGameState match {
      case Some(oldGameState) => {
        val newGameState = BasicRulesGame.makeMove(move, oldGameState)
        currentGameState = Some(newGameState)
        move match {
          case Pass(_) => {}
          case Put(stone) => {
            removeGhosts()
            // join to neighbour chains
            join(stone)
          }
        }
      }
      case None => {
        LOG.warning(s"$sender says, that game has already started, " +
          s"but I have no information about current state!")
        // TODO resign, die
      }
    }
  }

  def getGameState: GameState = {
    currentGameState match {
      case Some(x) => x
      case None => throw new IllegalStateException(s"Game is not started.")
    }
  }

  def getSubordinates = subordinates

  private def removeGhosts(): Unit = {
    val board = getGameState.board
    val ghosts = subordinates.getActors filter {
      subordinate => {
        val refPosition = subordinates.getReferenceStoneFor(subordinate)
        board(refPosition.position).isInstanceOf[FreeField]
      }
    }
    subordinates = subordinates - ghosts
    ghosts foreach (ghost => ghost ! PoisonPill)
  }

  private def join(stone: Stone) = {
    val board = getGameState.board
    val neighbourStones = game.getNeighbourStones(stone.position, stone.owner, board)
    val neighbourSubordinates = (for (stone <- neighbourStones) yield {
      val actorOption = subordinates.getSubordinateFor(stone.position)
      actorOption match {
        case Some(actor) => actor
        case None => throw new IllegalStateException(s"There is no actor for $stone")
      }
    }).toSet

    val chain = BasicRulesGame.getNonEmptyChain(stone.position, board)

    if (neighbourSubordinates.isEmpty) {
      val subordinate = createSubordinate(chain)
      subordinates = subordinates + Seq((subordinate, chain))
    }
    else if (neighbourSubordinates.size <= 1) {
      // Joining new stone to existing chain
      val subordinate = neighbourSubordinates.head
      subordinates = subordinates + Seq((subordinate, chain))
    } else {
      // Joining some existing chains into one
      // Current strategy - kill neighbours, create new one for larger chain
      // IDEA: maybe some other strategy? creating sets of actors?
      neighbourSubordinates foreach (neighbour => neighbour ! PoisonPill)
      subordinates = subordinates - neighbourSubordinates

      val subordinate = createSubordinate(chain)
      subordinates = subordinates + Seq((subordinate, chain))
    }
  }

  private def createSubordinate(chain: Chain): ActorRef = {
    if (chain.fields.isEmpty)
      throw new IllegalArgumentException(s"Chain should not be empty")

    val chainElement = chain.fields.head
    val newActor = chainElement.owner match {
      case `identity` => {
        actorOf(Soldier.props, createSubordinateName(Soldier.name, chainElement.position))
        // IDEA: creating aliases for other fields of chain?, for example, for one soldier there will be names: soldier_1_1 soldier_1_2, etc.
      }
      case _ => {
        actorOf(Spy.props, createSubordinateName(Spy.name, chainElement.position))
      }
    }
    // TODO send chain to actor
    newActor
  }

  private def createSubordinateName(subordType: String, position: Coords) = {
    s"${subordType}_${position.row}_${position.column}_${getGameState.history.moves.size}"
  }

  def getLogger: LoggingAdapter = LOG

}
