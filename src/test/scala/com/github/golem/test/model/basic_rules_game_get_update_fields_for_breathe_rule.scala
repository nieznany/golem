package com.github.golem.test.model

import com.github.golem.test.GolemUnitSpec
import com.github.golem.model._
import com.github.golem.model.Board._
import com.github.golem.model.Engine
import com.github.golem.model.Put
import com.github.golem.model.Board.Coords
import com.github.golem.model.Board.Stone
import com.github.golem.model.GameState.MovesHistory

class basic_rules_game_get_update_fields_for_breathe_rule extends GolemUnitSpec {
  val board1 = Board(Seq(".x..",
                         "xox.",
                         ".x.x",
                         "..xo"))
  val gameState1 = GameState(MovesHistory(List()), board1)

  val board2 = Board(Seq(".xox.",
                         "xxoxx",
                         "ooxoo",
                         "xxoxx",
                         ".xox."))

  val gameState2 = GameState(MovesHistory(List()), board2)

  "A game " should "select an opponents stone with one breath in center" in {
    val fields = BasicRulesGame getUpdatedFieldsForBreathRule(Put(Stone(Coords(3, 2), Engine)), gameState1)

    fields should contain (Free(Coords(2,2)))
  }

  "A game" should "select a stone from corner" in {
    val fields = BasicRulesGame getUpdatedFieldsForBreathRule(Put(Stone(Coords(4, 3), Engine)), gameState1)

    fields should contain (Free(Coords(4, 4)))
  }

  "A game" should "select all stones of opponent" in {
    val fields = BasicRulesGame getUpdatedFieldsForBreathRule(Put(Stone(Coords(3, 3), Engine)), gameState2)

    fields should contain allOf (Free(Coords(1, 3)), Free(Coords(2, 3)),
                                 Free(Coords(3, 1)), Free(Coords(3, 2)),
                                 Free(Coords(3, 4)), Free(Coords(3, 5)),
                                 Free(Coords(4, 3)), Free(Coords(5, 3)))
  }
}
