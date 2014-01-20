package com.github.golem.army.command

import com.github.golem.model.{Move, GameState}
import com.github.golem.army.model.Subordinates
import akka.actor.ActorRef
import com.github.golem.model.BasicRulesGame.Chain

// 'None' means - "going somewhere else, chains on board are not important now"
object SuggestMove {
  case class Response(move: Move, objective: Objective)
}

case class SuggestMove(state: GameState, privates: Subordinates, captains: Subordinates)
