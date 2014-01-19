package com.github.golem.test

import com.github.golem.model.{Human, Engine, BasicRulesGame, Board}
import com.github.golem.model.Board.{Free, Stone, Coords}

class basic_rules_bensons_algorith_test extends GolemUnitSpec{

  val board1 = Board(Seq(
    "x.x.x.",
    ".xx..x",
    "xxo..x",
    "x....x",
    "x.x.xx",
    "xx.xx."))

  val board2 = Board(Seq(
    "oo..",
    "xx..",
    "x.xx",
    "..x."))

  "A game " should "select field's neighbours except for stones of the same player" in {
    val fields = BasicRulesGame getRegionNeighbourFields(Coords(2, 2), Engine, board2)

    fields should contain theSameElementsAs Vector(Stone(Coords(1, 2), Human), Free(Coords(2, 3)), Free(Coords(3, 2)))
  }

  "A game " should "select chain's neighbours except for stones of the same player " in {
    val chain = BasicRulesGame getNonEmptyChain(Coords(2, 2), board2)
    val fields = BasicRulesGame getChainNeighbourFields(chain, Engine, board2)

    fields should contain theSameElementsAs Vector(Coords(1, 1), Coords(1, 2), Coords(2, 3), Coords(3, 2), Coords(4, 1))
  }

  "A game" should "tell if given group of chains is alive" in {
    val chain1 = BasicRulesGame getNonEmptyChain(Coords(1, 1), board1)
    val chain2 = BasicRulesGame getNonEmptyChain(Coords(2, 2), board1)
    val chain3 = BasicRulesGame getNonEmptyChain(Coords(5, 3), board1)
    val chain4 = BasicRulesGame getNonEmptyChain(Coords(6, 4), board1)
    val chain5 = BasicRulesGame getNonEmptyChain(Coords(1, 5), board1)
    var resFalse = BasicRulesGame isGroupAlive(Set(chain1, chain2, chain3, chain4, chain5), board1)
    var resTrue = BasicRulesGame isGroupAlive(Set(chain1, chain2), board1)
    for (i <- (1 to 1000)) {
      resFalse = BasicRulesGame isGroupAlive(Set(chain1, chain2, chain3, chain4, chain5), board1)
    }
    resFalse should be(false);
    resTrue should be(true)
  }
}
