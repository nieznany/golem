package com.github.golem.test.model

import com.github.golem.test.GolemUnitSpec
import com.github.golem.model._
import com.github.golem.model.Board._
import com.github.golem.model.Engine
import com.github.golem.model.Put
import com.github.golem.model.Board.Coords
import com.github.golem.model.Board.Stone
import com.github.golem.model.GameState.MovesHistory

class basic_rules_game_get_endangered_stones extends GolemUnitSpec {
  var board1 = Board(Seq(
    ".x..",
    "xox.",
    "...x",
    "...o"))

  board1 = BasicRulesGame.decomposeBoard(board1)

  var board2 = Board(Seq(
    ".xox.",
    "xxoxx",
    "oo.oo",
    "xxoxx",
    ".xox."))
  board2 = BasicRulesGame.decomposeBoard(board2)

  "A game " should "select an opponents stone with one breath in center" in {
    val fields = BasicRulesGame getEndangeredStones(Put(Stone(Coords(3, 2), Engine)), board1)

    fields should contain theSameElementsAs Vector(Stone(Coords(2, 2), Human))
  }

  "A game" should "select a stone from corner" in {
    val fields = BasicRulesGame getEndangeredStones(Put(Stone(Coords(4, 3), Engine)), board1)

    fields should contain theSameElementsAs Vector(Stone(Coords(4, 4), Human))
  }

  "A game" should "select all stones of opponent" in {
    val fields = BasicRulesGame getEndangeredStones(Put(Stone(Coords(3, 3), Engine)), board2)

    fields should contain theSameElementsAs Vector(Stone(Coords(1, 3), Human), Stone(Coords(2, 3), Human),
      Stone(Coords(3, 1), Human), Stone(Coords(3, 2), Human),
      Stone(Coords(3, 4), Human), Stone(Coords(3, 5), Human),
      Stone(Coords(4, 3), Human), Stone(Coords(5, 3), Human))
  }
}
