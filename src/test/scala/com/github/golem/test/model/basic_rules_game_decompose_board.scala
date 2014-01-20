package com.github.golem.test.model

import com.github.golem.model.{Human, BasicRulesGame, Board}
import com.github.golem.test.GolemUnitSpec
import org.scalatest.Matchers._
import com.github.golem.model.Board.{FreeField, Stone, Coords}
import com.github.golem.model.BasicRulesGame.{Group,Chain}

class basic_rules_game_decompose_board extends GolemUnitSpec {

  // TODO remove or write appropriate assertions

  "A game" should "decompose board into two groups" in {
    val board1 = Board(Seq(
      "....x",
      "o....",
      ".o..x"))
  }

  "A game" should "decompose into 3 groups" in {
    val board1 = Board(Seq(
      "x..xx",
      "o....",
      ".o..x"))
    println(BasicRulesGame.decomposeBoard(board1))
  }
}
