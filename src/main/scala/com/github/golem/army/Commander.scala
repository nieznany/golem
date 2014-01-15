package com.github.golem.army

import akka.actor.{PoisonPill, ActorRef, Props, Actor}
import com.github.golem.model._
import com.github.golem.command.game.{GenerateMove, MadeMove}
import com.github.golem.command.Informative
import com.github.golem.command.Command
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
import com.github.golem.model.BasicRulesGame.Chain
import com.github.golem.army.model.Subordinates
import com.github.golem.army.command.{Objective, SuggestMove}
import scala.concurrent.{Await, Future}
import akka.util.Timeout

object Commander {
  def props = Props(classOf[Commander])
}

class Commander extends GolemActor {

  import context._

  /** Identity of commander */
  private val LOG = Logging(system, this)
  // IDEA: handle other types of rules
  private var currentGameState: Option[GameState] = None
  private var subordinates: Subordinates = new Subordinates

  implicit val timeout = Timeout(10 seconds)

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
            LOG.info(s"Game is finished, result state: $currentGameState")
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
            val answersFutureList = Future.traverse(subordinates.getActors) {
              actor => actor.ask(SuggestMove(getGameState, subordinates))
            }
            val result = Await.result(answersFutureList, timeout.duration)

            val moves = result filter {
              result => result.isInstanceOf[SuggestMove.Response]
            } map {
              result => result.asInstanceOf[SuggestMove.Response]
            } // FIX inefficient, how to filter and map at once?

            val bestMove = (moves + suggestMove) reduceLeft {
              (m1, m2) => {
                chooseBetter(m1, m2)
              }
            }
            sender ! GenerateMove.Response(bestMove.move)

            updateFor(bestMove.move)
            LOG.info(s"New game state: $currentGameState") // TODO change to debug
            LOG.info(s"$subordinates")
          }
        }
      }
      case ufo => {
        LOG.warning(s"I saw UFO: $ufo. I will ignore it...")
      }
    }
  }

  def chooseBetter(answer1: SuggestMove.Response, answer2: SuggestMove.Response): SuggestMove.Response = answer1// TODO change that

  def suggestMove: SuggestMove.Response = {
    SuggestMove.Response(Pass(identity), Objective(None)) // TODO change that
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
