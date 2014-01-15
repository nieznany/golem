package com.github.golem.army

import akka.actor.{Props, Actor}
import akka.event.{LoggingAdapter, Logging}
import com.github.golem.army.command.{Objective, SuggestMove}
import com.github.golem.model._
import com.github.golem.model.Board.{Stone, Free}
import com.github.golem.model.Put
import com.github.golem.model.Board.Free
import com.github.golem.model.Pass
import scala.Some
import com.github.golem.model.Board.Stone

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
        // TODO mniej trywialne rozwiazanie
        val referenceStone = siblings.getReferenceStoneFor(self)
        val currentBoard = gameState.board
        val myChain = game.getNonEmptyChain(referenceStone.position, currentBoard)
        val freeBreath = myChain.breaths find (field => field.isInstanceOf[Free])
        val myMove = freeBreath match {
          case Some(freeField) => Put(Stone(freeField.position, identity))
          case None => Pass(identity)
        }
        sender ! SuggestMove.Response(myMove, Objective(Some(myChain)))
      }
    }
  }
}
