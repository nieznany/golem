package com.github.golem.test.model

import com.github.golem.test.GolemUnitSpec
import com.github.golem.model._
import com.github.golem.model.Board.{Stone, Coords, Unavailable}
import com.github.golem.model.Board.Coords
import com.github.golem.model.Put
import com.github.golem.model.Board.Stone
import com.github.golem.model.GameState.MovesHistory

class basic_rules_game_make_move_test extends GolemUnitSpec {

  "A game" should "remove one stone" in {
    val board = Board(Seq(
      ".o..",
      "ox..",
      ".o.."))
    val move = Put(Stone(Coords(2, 3), Human))
    val gameState = new GameState(board)

    val newState = BasicRulesGame.makeMove(Pass(Engine), gameState)
    val newState2 =  BasicRulesGame.makeMove(move, gameState)
    newState2.board should be (Board(Seq(
      "Xo..",
      "oXo.",
      "Xo.."
    )))
  }

  "A game" should "recognize death eyes" in {
    val board = Board(Seq(
      "ooooo",
      "oxxxo",
      "ox.xo",
      "oxxxo",
      "ooooo"))
    val move = Put(Stone(Coords(3, 3), Human))

    val gameState = new GameState(board)

    val newState = BasicRulesGame.makeMove(move, gameState)
    newState.history.moves should contain theSameElementsAs Vector(move)
    newState.board should be(Board(Seq(
      "ooooo",
      "o...o",
      "o.o.o",
      "o...o",
      "ooooo")))
  }

  "A game" should "apply Ko rule" in {
    val board = Board(Seq(
      ".xo..",
      "x.xo.",
      ".xo.."))

    val move = Put(Stone(Coords(2, 2), Human))
    val move2 = Put(Stone(Coords(1, 5), Engine))
    val move3 = Pass(Human)

    val gameState = new GameState(board)

    val newState = BasicRulesGame.makeMove(move, gameState)
    newState.board should be(Board(Seq(
      ".xo..",
      "xoXo.",
      ".xo..")))

    val newState2 = BasicRulesGame.makeMove(move2, newState)
    newState2.board should be(Board(Seq(
      ".xo.x",
      "xo.o.",
      ".xo..")))

    val newState3 = BasicRulesGame.makeMove(move3, newState2)
    newState3.board should be(Board(Seq(
      ".xo.x",
      "xo.o.",
      ".xo..")))
  }

  "A game" should "persist unavailable fields after pass" in {
    val board = Board(Seq(
      ".o..",
      "o...",
      ".o.."))

    val move = Put(Stone(Coords(2, 3), Human))
    val move2 = Pass(Engine)
    val move3 = Pass(Human)
    val gameState = new GameState(board)

    val newState = BasicRulesGame.makeMove(move, gameState)
    newState.board should be(Board(Seq(
      "Xo..",
      "oXo.",
      "Xo.."
    )))

    val newState2 = BasicRulesGame.makeMove(move2, newState)
    newState2.board should be(Board(Seq(
      "Xo..",
      "oXo.",
      "Xo.."
    )))

    val newState3 = BasicRulesGame.makeMove(move3, newState2)
    newState3.board should be(Board(Seq(
      "Xo..",
      "oXo.",
      "Xo.."
    )))
  }
}
