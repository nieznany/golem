package com.github.golem.test

import akka.testkit.TestActorRef
import com.github.golem.army.Captain
import com.github.golem.model.{BasicRulesGame, Board}
import com.github.golem.model.Board.{Free, Coords}

class captain_test extends GolemActorUnitSpec {
  val board1 = Board(Seq(
    "x.x.x.",
    ".xx..x",
    "xxo..x",
    "x....x",
    "x.x.xx",
    "xx.xx."))

  val board2 = Board(Seq(
    "x...",
    "xx..",
    ".o..",
    "...."))

  val board3 = Board(Seq(
    "..x...",
    ".xx...",
    "xxo..x",
    ".....x",
    ".....x",
    "......"))


  "A game" should "should find group available coords" in {

    val chain1 = BasicRulesGame getNonEmptyChain(Coords(1, 1), board2)
    val coords = BasicRulesGame getGroupAvailablePositions(Set(chain1), board2)

    coords should contain theSameElementsAs (Vector(Coords(1, 2), Coords(1, 3), Coords(3, 1), Coords(4, 1), Coords(2, 3),
      Coords(2, 4), Coords(3, 3)))

  }
  "A game" should "suggest best move for group" in {
    val testCaptain = TestActorRef(new Captain, "captain1")
    val captain = testCaptain.underlyingActor

    val chain1 = BasicRulesGame getNonEmptyChain(Coords(1, 3), board3)
    val chain2 = BasicRulesGame getNonEmptyChain(Coords(3, 6), board3)


    var move1: (Option[Free], Int) = (None, -1)
    var move2: (Option[Free], Int) = (None, -1)
    for (i <- 1 to 100) {
      move1 = captain.getBestMoveForGroup(Set(chain1), board3)
      move2 = captain.getBestMoveForGroup(Set(chain2), board3)
    }

    move1 should be(Some(Free(Coords(1, 1))), 8)
    move2 should be(None, 0)
  }

}
