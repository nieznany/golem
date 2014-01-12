package com.github.golem.test.model

import com.github.golem.model.{Human, Engine, BasicRulesGame, Board}
import com.github.golem.test.GolemUnitSpec
import com.github.golem.model.Board.{Unavailable, Coords, Free}
import scala.collection.immutable.Vector

class basic_rules_get_unavailable_fields_test extends GolemUnitSpec {

  "A game" should "declare all unavailable fields for player" in {
    val board = Board(Seq(
      ".x..",
      "x.x.",
      ".x.x",
      "..x."))
    BasicRulesGame.getUnavailableFields(Human, board) should contain theSameElementsAs Vector(
      Unavailable(Coords(1, 1), Human),
      Unavailable(Coords(2, 2), Human),
      Unavailable(Coords(3, 3), Human),
      Unavailable(Coords(4, 4), Human))
  }

  "A game" should "declare available fields, which kills opponents stone" in {
    val board = Board(Seq(
      "..x.",
      "..ox",
      "xo..",
      ".xo."))
    BasicRulesGame.getUnavailableFields(Human, board) should contain theSameElementsAs Vector(
      Unavailable(Coords(1, 4), Human))
  }

  "A game" should "declare available fields, which kills opponents stone (2)" in {
    val board = Board(Seq(
      "..x.",
      "..ox",
      "xo.o",
      ".xo."))
    BasicRulesGame.getUnavailableFields(Human, board) shouldBe empty
    BasicRulesGame.getUnavailableFields(Engine, board) should contain theSameElementsAs Vector(
      Unavailable(Coords(3, 3), Engine),
      Unavailable(Coords(4, 4), Engine)
    )
  }

  "A game" should "disable suicide move" in {
    val board = Board(Seq(
      "ooooo",
      "oxxxo",
      "ox.xo",
      "oxxxo",
      "ooooo"))

    BasicRulesGame.getUnavailableFields(Engine, board) should contain theSameElementsAs Vector(
      Unavailable(Coords(3,3), Engine)
    )
  }

  "A game" should "should distinguish suicide and attack move" in {
    val board = Board(Seq(
      "xoo",
      "x.o",
      "xxo"))
    BasicRulesGame.getUnavailableFields(Human, board) shouldBe empty
    BasicRulesGame.getUnavailableFields(Engine, board) shouldBe empty
  }

  "A game" should "should distinguish suicide and attack move (2)" in {
    val board = Board(Seq(
      "..ooo.",
      ".oxxxo",
      "ox.oox",
      "oxoxx.",
      "oxox..",
      ".ox..."))

    BasicRulesGame.getUnavailableFields(Engine, board) shouldBe empty
    BasicRulesGame.getUnavailableFields(Engine, board) shouldBe empty
  }
}
