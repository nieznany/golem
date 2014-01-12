package com.github.golem.test.model

import com.github.golem.test.GolemUnitSpec
import com.github.golem.model.{Human, Engine, BasicRulesGame, Board}
import com.github.golem.model.Board.{Stone, Coords}
import org.scalatest._
import Matchers._

class basic_rules_game_get_chain_test extends GolemUnitSpec {
  val board1 = Board(Seq("x.x.x",
                         "oo.xx",
                         ".ooox"))

   "A game" should "declare long chain of engine stones" in {
      val chain = BasicRulesGame getChain (Coords(2, 4), board1)

      chain.value.nbreaths should be (2)
      chain.value.fields should contain allOf (Stone(Coords(1,5), Engine),
                                            Stone(Coords(2,4), Engine),
                                            Stone(Coords(2,5), Engine),
                                            Stone(Coords(3,5), Engine))
   }
  it should "not declare chain in empty field" in {
      val chain = BasicRulesGame getChain(Coords(1,4), board1)
      chain should be (None)
  }

  it should "declare chain for one stone next border" in {
      val chain = BasicRulesGame getChain (Coords(1, 3), board1)

      chain.value.nbreaths should be (3)
      chain.value.fields should contain (Stone(Coords(1, 3), Engine))
  }

  it should "declare chain for one stone in corner" in {
      val chain = BasicRulesGame getChain (Coords(1, 1), board1)

      chain.value.nbreaths should be (1)
      chain.value.fields should contain (Stone(Coords(1, 1), Engine))
  }

  it should "declare chain for other player" in {
    val chain = BasicRulesGame getChain (Coords(2, 2), board1)

    chain.value.nbreaths should be (3)
    chain.value.fields should contain allOf (Stone(Coords(2, 1), Human),
                                         Stone(Coords(2, 2), Human),
                                         Stone(Coords(3, 2), Human),
                                         Stone(Coords(3, 3), Human),
                                         Stone(Coords(3, 4), Human))
  }
}
