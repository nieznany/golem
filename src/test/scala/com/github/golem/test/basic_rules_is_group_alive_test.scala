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

  val board3 = Board(Seq(
    "..x...",
    ".xx...",
    "xxo..x",
    ".....x",
    ".....x",
    "......"))

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

    val chain6 = BasicRulesGame getNonEmptyChain(Coords(1, 3), board3)

    var res1 = BasicRulesGame getGroupLives(Set(chain1, chain2, chain3, chain4, chain5), board1)
    var res2 = BasicRulesGame getGroupLives(Set(chain3, chain4,chain5), board1)
    var res3 = BasicRulesGame getGroupLives(Set(chain6), board3)

    for (i <- (1 to 1000)) {
      res1 = BasicRulesGame getGroupLives(Set(chain1, chain2, chain3, chain4, chain5), board1)
    }
    res1 should be(12);
    res2 should be(0)
    res3 should be(0)
  }
}
