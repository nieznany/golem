package com.github.golem.test

import com.github.golem.model._
import akka.testkit.TestActorRef
import com.github.golem.army.Soldier
import com.github.golem.model.Board.Free
import com.github.golem.model.Board.Coords

class soldier_test extends GolemActorUnitSpec {
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

  "A Soldier" should "suggest best move accordingly to max liberties increase" in {
    val testActor = TestActorRef(new Soldier, "soldier1")
    val soldier = testActor.underlyingActor
    val chain1 = BasicRulesGame getNonEmptyChain(Coords(2, 6), board1)
    val chain2 = BasicRulesGame getNonEmptyChain(Coords(2, 2), board1)
    val chain3 = BasicRulesGame getNonEmptyChain(Coords(2, 2), board2)

    val move1 = soldier getBestMove(chain1, board1)
    val move2 = soldier getBestMove(chain2, board1)
    val move3 = soldier getBestMove(chain3, board2)

    move1 should be(Some(Free(Coords(3, 5))))
    move2 should be(Some(Free(Coords(2, 4))))
    move3 should be(Some(Free(Coords(2, 3))))
  }

}
