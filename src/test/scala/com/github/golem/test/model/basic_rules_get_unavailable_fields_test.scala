package com.github.golem.test.model

import com.github.golem.model.{Human, Engine, BasicRulesGame, Board}
import com.github.golem.test.GolemUnitSpec
import com.github.golem.model.Board.{FreeField, Unavailable, Coords, Free}
import scala.collection.immutable.Vector

class basic_rules_get_unavailable_fields_test extends GolemUnitSpec {

  "A game" should "declare all unavailable fields for player" in {
    val board = Board(Seq(
      ".x..",
      "x.x.",
      ".x.x",
      "..x."))
    getOnlyUnavailable(BasicRulesGame.updateAvailabilityOfFields(Human, board)) should contain theSameElementsAs Vector(
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
    getOnlyUnavailable(BasicRulesGame.updateAvailabilityOfFields(Human, board)) should contain theSameElementsAs Vector(
      Unavailable(Coords(1, 4), Human))
  }

  "A game" should "declare available fields, which kills opponents stone (2)" in {
    val board = Board(Seq(
      "..x.",
      "..ox",
      "xo.o",
      ".xo."))
    getOnlyUnavailable(BasicRulesGame.updateAvailabilityOfFields(Human, board)) shouldBe empty
    getOnlyUnavailable(BasicRulesGame.updateAvailabilityOfFields(Engine, board)) should contain theSameElementsAs Vector(
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

    getOnlyUnavailable(BasicRulesGame.updateAvailabilityOfFields(Engine, board)) should contain theSameElementsAs Vector(
      Unavailable(Coords(3,3), Engine)
    )
  }

  "A game" should "should distinguish suicide and attack move" in {
    val board = Board(Seq(
      "xoo",
      "x.o",
      "xxo"))
    getOnlyUnavailable(BasicRulesGame.updateAvailabilityOfFields(Human, board)) shouldBe empty
    getOnlyUnavailable(BasicRulesGame.updateAvailabilityOfFields(Engine, board)) shouldBe empty
  }

  "A game" should "should distinguish suicide and attack move (2)" in {
    val board = Board(Seq(
      "..ooo.",
      ".oxxxo",
      "ox.oox",
      "oxoxx.",
      "oxox..",
      ".ox..."))

    getOnlyUnavailable(BasicRulesGame.updateAvailabilityOfFields(Engine, board)) shouldBe empty
    getOnlyUnavailable(BasicRulesGame.updateAvailabilityOfFields(Engine, board)) shouldBe empty
  }

  private def getOnlyUnavailable(fields: Set[FreeField]): Set[Unavailable] = {
    fields filter {field => field.isInstanceOf[Unavailable]} map {field => field.asInstanceOf[Unavailable]}
  }
}
