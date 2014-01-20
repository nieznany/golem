package com.github.golem.army

import akka.actor.{PoisonPill, ActorRef, Props, Actor}
import com.github.golem.model._
import com.github.golem.command.game.{GenerateMove, MadeMove}
import com.github.golem.command.{EmptyResponse, Informative, Command}
import akka.event.{LoggingAdapter, Logging}
import akka.dispatch._
import scala.concurrent.{ExecutionContext, Await, Future}
import ExecutionContext.Implicits.global
import akka.pattern.ask
import scala.concurrent.duration._
import com.github.golem.command.setup.{SetKomi, QuitGame, SetBoardSize}
import com.github.golem.model.Pass
import scala.Some
import com.github.golem.model.Board.{FreeField, Stone, Coords}
import com.github.golem.model.BasicRulesGame.{Group, Chain}
import com.github.golem.army.model.Subordinates
import com.github.golem.army.command._
import scala.concurrent.{Await, Future}
import akka.util.Timeout
import com.github.golem.model.Board._
import com.github.golem.command.tournament.DeadFinalStatusList
import com.github.golem.army.command.SuggestMove.Response
import com.github.golem.model.BasicRulesGame.Group
import com.github.golem.command.setup.SetBoardSize
import com.github.golem.command.setup.SetKomi
import scala.Some
import com.github.golem.model.Put
import com.github.golem.army.command.Attack
import com.github.golem.model.Board.Coords
import com.github.golem.army.command.SuggestMove.Response
import com.github.golem.command.game.MadeMove
import com.github.golem.model.BasicRulesGame.Chain
import com.github.golem.army.command.Defense
import com.github.golem.model.Pass
import com.github.golem.army.model.Subordinates
import com.github.golem.model.Board.Stone

object Commander {
  def props = Props(classOf[Commander])
}

// TODO this class is huge, split it!
class Commander extends GolemActor {

  import context._

  /** Identity of commander */
  private val LOG = Logging(system, this)
  // IDEA: handle other types of rules
  private var currentGameState: Option[GameState] = None
  private var privates: Subordinates = new Subordinates
  private var captains: Subordinates = new Subordinates

  // Greater number - higher priority
  private val movesPriorities: Map[Class[_ <: Move], Int] = Map[Class[_ <: Move], Int](
    (classOf[Put] -> 1000), classOf[Pass] -> 0)
  private val objectivePriorities: Map[Class[_ <: Objective], Int] = Map[Class[_ <: Objective], Int](
    (classOf[Defense] -> 100), (classOf[Attack] -> 90), ((classOf[Fun] -> 1)), classOf[Despair] -> 0)

  object ObjectiveOrdering extends Ordering[Objective] {
    def compare(x: Objective, y: Objective): Int = {
      val classPriorirtyDiff = objectivePriorities(x.getClass) - objectivePriorities(y.getClass)
      if (classPriorirtyDiff == 0) {
        // Classes with the same priority
        if (x.getClass != y.getClass) {
          throw new UnsupportedOperationException("Comparison between different types of objectives is currently not supported.")
        }
        x match {
          case defenseX: PrivatesObjective => {
            val defenseY = y.asInstanceOf[PrivatesObjective]
            val priority1 = -(defenseX.nbreathsLeft compareTo (defenseY.nbreathsLeft)) // more important
            if (priority1 == 0) {
              return defenseX.nstones compareTo (defenseY.nstones) // less important
            }
            else return priority1
          }
          case ufo => {
            throw new IllegalArgumentException(s"Unsupported type of objective: $ufo")
          }
        }
      }
      else return classPriorirtyDiff
    }
  }

  object MoveOrdering extends Ordering[Move] {
    def compare(x: Move, y: Move): Int = {
      movesPriorities(x.getClass) - movesPriorities(y.getClass)
    }
  }

  object ResponseOrdering extends Ordering[Response] {
    def compare(x: Response, y: Response): Int = {
      if (MoveOrdering.compare(x.move, y.move) == 0) {
        ObjectiveOrdering.compare(x.objective, y.objective)
      }
      else MoveOrdering.compare(x.move, y.move)
    }
  }

  implicit val timeout = Timeout(10 seconds)


  def handle(message: Any) = {
    message match {
      case i: Informative => {
        i match {
          case SetBoardSize(size) => {
            // Start new game
            currentGameState = Some(new GameState(Board(size, size)))
            privates = Subordinates()
            captains = Subordinates()
            LOG.info(s"Starting new game: $currentGameState")
          }
          case SetKomi(_) => {
            LOG.info("Ignoring information about Komi.")
          }

          case QuitGame => {
            LOG.info(s"Game is finished, result state: $currentGameState")
            privates.getActors foreach (subordinate => subordinate ! PoisonPill)
            currentGameState = None
            privates = Subordinates()
            captains = Subordinates()
          }
          case MadeMove(move) => {
            updateFor(move)
            LOG.info(s"New game state: $currentGameState") // TODO change to debug
            LOG.info(s"Privates: $privates")
            LOG.info(s"Captains: $captains")
          }
        }
      }
      case cmd: Command => {
        cmd match {
          case GenerateMove => {
            val answersFutureList = Future.traverse(getSubordinates) {
              actor => actor.ask(SuggestMove(getGameState, privates, captains))
            }
            val result = Await.result(answersFutureList, timeout.duration) // Czekaj na wszystkich aktorów dany czas

            val responses = result filter {
              result => result.isInstanceOf[SuggestMove.Response]
            } map {
              result => result.asInstanceOf[SuggestMove.Response]
            } // FIX inefficient, how to filter and map at once?

            val bestResponse = getBestResponse(responses)
            sender ! GenerateMove.Response(bestResponse.move)

            updateFor(bestResponse.move)
            LOG.info(s"New game state: $currentGameState") // TODO change to debug
            LOG.info(s"$privates")
            LOG.info(s"$captains")
          }
          case DeadFinalStatusList => {
            sender ! EmptyResponse
          }
        }
      }
      case ufo => {
        LOG.warning(s"I saw UFO: $ufo. I will ignore it...")
      }
    }
  }

  def getSubordinates: Set[ActorRef] = privates.getActors.union(captains.getActors)

  def getBestResponse(responses: Iterable[Response]): Response = {
    responses max ResponseOrdering
  }

  def suggestMove: SuggestMove.Response = {
    SuggestMove.Response(Pass(identity), Fun())
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
            removeGhostPrivates()
            removeOrSplitCaptains(stone.owner.opponent())
            // join to neighbour chains
            joinPrivate(stone)
            joinCaptain(stone)
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

  def getPrivates = privates

  private def removeGhostPrivates() = {
    val board = getGameState.board
    val ghosts = privates.getActors filter {
      subordinate => {
        val refPosition = privates.getReferenceStoneFor(subordinate)
        board(refPosition.position).isInstanceOf[FreeField]
      }
    }
    ghosts foreach (ghost => ghost ! PoisonPill)
    privates -= ghosts
  }

  private def removeOrSplitCaptains(player: Player) {
    val board = getGameState.board
    // We consider only groups, which belongs to opponent of last move's owner
    // There is not chance to split captains of current player(they may only join together)
    val playerCaptains = captains.getActors filter {
      actor => captains.getReferenceStoneFor(actor).owner == player
    }
    for (captain <- playerCaptains) {
      val captainCoords = captains.getCoordsForSubordinate(captain)
      val captainGroups = scala.collection.mutable.Set[Group]()
      for (coord <- captainCoords) {
        board.getDecomposedGroup(coord) match {
          case Some(group) => captainGroups += group
          case None => {} // There is no more group for this captain
        }
      }
      if (captainGroups.isEmpty) {
        // Captain is death!
        killCaptain(captain)
      }
      else if (captainGroups.size == 1) {
        // just for sure, refresh captain's fields
        // TODO how to change name od already created captain? The name of captain can be misleading, because may point to non-existing stone
        captains -= captain
        captains = captains.addGroups(Seq((captain, captainGroups.head)))
      }
      else if (captainGroups.size > 1) {
        // We need to split new groups between captains.
        killCaptain(captain)
        for (group <- captainGroups) {
          val newCaptain = createCaptain(group)
          captains = captains.addGroups(Seq((newCaptain, group)))
        }
      }
    }
  }

  private def killCaptain(captain: ActorRef) {
    captains -= captain
    captain ! PoisonPill
  }

  private def joinPrivate(stone: Stone) = {
    val board = getGameState.board
    val neighbourStones = game.getNeighbourStones(stone.position, stone.owner, board)
    val neighbourSubordinates = (for (stone <- neighbourStones) yield {
      val actorOption = privates.getSubordinateFor(stone.position)
      actorOption match {
        case Some(actor) => actor
        case None => throw new IllegalStateException(s"There is no actor for $stone")
      }
    }).toSet

    val chain = board.getDecomposedNonEmptyChain(stone.position)

    if (neighbourSubordinates.isEmpty) {
      val subordinate = createPrivate(chain)
      privates = privates + Seq((subordinate, chain))
    }
    else if (neighbourSubordinates.size <= 1) {
      // Joining new stone to existing chain
      val subordinate = neighbourSubordinates.head
      privates = privates + Seq((subordinate, chain))
    } else {
      // Joining some existing chains into one
      // Current strategy - kill neighbours, create new one for larger chain
      // IDEA: maybe some other strategy? creating sets of actors?
      neighbourSubordinates foreach (neighbour => neighbour ! PoisonPill)
      privates = privates - neighbourSubordinates

      val subordinate = createPrivate(chain)
      privates = privates + Seq((subordinate, chain))
    }
  }

  private def createPrivate(chain: Chain): ActorRef = {
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
    newActor
  }

  // TODO refactor
  private def joinCaptain(stone: Stone) = {
    val board = getGameState.board
    val neighbourCaptains = scala.collection.mutable.Set[ActorRef]()
    game.DIRECTIONS foreach {
      direction => // Possible two step neighbourhood
        val neighbourPosition = stone.position + direction
        val actorOption = captains.getSubordinateFor(neighbourPosition)
        actorOption match {
          case Some(actor) => {
            board(neighbourPosition) match {
              case Stone(_, stone.owner) => neighbourCaptains += actor
              case _ => {}
            }
          }
          case None => {
            // Try to get actor 1 field further
            val furtherPosition = stone.position + direction + direction
            if (!board.isOutOfBounds(furtherPosition.row, furtherPosition.column)) {
              captains.getSubordinateFor(furtherPosition) match {
                case Some(furtherActor) => {
                  board(neighbourPosition) match {
                    case _: FreeField => {
                      // groups are separated by free fields.
                      board(furtherPosition) match {
                        // FIXME duplication of code
                        case Stone(_, stone.owner) => neighbourCaptains += furtherActor
                        case _ => {}
                      }
                    }
                    case _ => {}
                  }
                }
                case None => {}
              }
            }
          }
        }
    }

    List(N + E, S + E, N + W, S + W) foreach {
      direction => // Possible 1-step neighbourhood
        val newPosition = stone.position + direction
        captains.getSubordinateFor(newPosition) match {
          case Some(actor) => {
            board(newPosition) match {
              // FIXME duplication of code
              case Stone(_, stone.owner) => neighbourCaptains += actor
              case _ => {}
            }
          }
          case None => {}
        }
    }

    val group = board.getDecomposedNonEmptyGroup(stone.position)

    if (neighbourCaptains.isEmpty) {
      val captain = createCaptain(group)
      captains = captains addGroups Seq((captain, group))
    }
    else if (neighbourCaptains.size <= 1) {
      val captain = neighbourCaptains.head
      captains = captains addGroups Seq((captain, group))
    } else {
      neighbourCaptains foreach (neighbour => neighbour ! PoisonPill)
      captains = captains - neighbourCaptains.toSet

      val captain = createCaptain(group)
      captains = captains addGroups Seq((captain, group))
    }
  }

  private def createCaptain(group: Group): ActorRef = {
    val chainElement = group.chains.head.fields.head
    val newActor = chainElement.owner match {
      case `identity` => {
        actorOf(Captain.props, createSubordinateName(Captain.name, chainElement.position))
        // IDEA: creating aliases for other fields of chain?, for example, for one soldier there will be names: soldier_1_1 soldier_1_2, etc.
      }
      case _ => {
        actorOf(SpyLeader.props, createSubordinateName(SpyLeader.name, chainElement.position))
      }
    }
    newActor
  }

  private def createSubordinateName(subordType: String, position: Coords) = {
    s"${subordType}_${position.row}_${position.column}_${getGameState.history.moves.size}"
  }

  def getLogger: LoggingAdapter = LOG


}
