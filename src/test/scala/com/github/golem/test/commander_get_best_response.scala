package com.github.golem.test

import com.github.golem.army.Commander
import com.github.golem.army.command._
import com.github.golem.model.{Pass, Engine, Human, Put}
import com.github.golem.model.Board.{Stone, Coords}
import org.scalatest.Matchers._
import akka.testkit.TestActorRef
import com.github.golem.army.command.Defense
import com.github.golem.model.Put
import com.github.golem.army.command.Fun
import com.github.golem.army.command.Attack
import com.github.golem.model.Board.Coords
import com.github.golem.model.Board.Stone

class commander_get_best_response extends GolemActorUnitSpec {
  val commanderRef = TestActorRef(new Commander)
  val commander = commanderRef.underlyingActor

  "A commander" should "respect selected ordering fo captains" in {
    val suggestions = Set[SuggestMove.Response](
      SuggestMove.Response(Put(Stone(Coords(1,2), Engine)), AttackGroup(5)),
        SuggestMove.Response(Put(Stone(Coords(1,2), Engine)), DefendGroup(3))
    )
    commander.getBestResponse(suggestions).objective should be(AttackGroup(5))
  }

  "A commander" should "respect selected priorities of captains" in {
    val suggestions = Set[SuggestMove.Response](
    SuggestMove.Response(Put(Stone(Coords(1,2), Engine)), Attack(3, 7)),
    SuggestMove.Response(Put(Stone(Coords(1,2), Engine)), DefendGroup(5))
    )
    commander.getBestResponse(suggestions).objective should be(DefendGroup(5))
  }

  "A commander" should "respect selected priorities of objectives" in {
    val suggestions = Set[SuggestMove.Response](
    SuggestMove.Response(Put(Stone(Coords(1,1), Engine)), Fun()),
    SuggestMove.Response(Put(Stone(Coords(1,2), Engine)), Attack(3, 7))
    )
    commander.getBestResponse(suggestions).objective should be(Attack(3,7))
  }

  "A commander" should "should respect privates breaths ordering" in {
    val suggestions = Set[SuggestMove.Response](
    SuggestMove.Response(Put(Stone(Coords(1,1), Engine)), Despair()),
    SuggestMove.Response(Put(Stone(Coords(1,2), Engine)), Attack(3, 7)),
    SuggestMove.Response(Put(Stone(Coords(5,2), Engine)), Attack(4, 6))
    )
    commander.getBestResponse(suggestions).objective should be(Attack(4,6))
  }

  "A commander" should "should respect privates number of fields ordering" in {
    val suggestions = Set[SuggestMove.Response](
    SuggestMove.Response(Put(Stone(Coords(1,1), Engine)), Despair()),
    SuggestMove.Response(Put(Stone(Coords(1,2), Engine)), Attack(5, 6)),
    SuggestMove.Response(Put(Stone(Coords(5,2), Engine)), Attack(4, 6))
    )
    commander.getBestResponse(suggestions).objective should be(Attack(5, 6))
  }

  "A commander" should "should respect moves priorities" in {
    val suggestions = Set[SuggestMove.Response](
    SuggestMove.Response(Put(Stone(Coords(1,1), Engine)), Despair()),
    SuggestMove.Response(Pass(Engine), Fun()),
    SuggestMove.Response(Pass(Engine), Fun())
    )
    commander.getBestResponse(suggestions).objective should be(Despair())
  }
}
