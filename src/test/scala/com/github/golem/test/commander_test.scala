package com.github.golem.test

import akka.testkit.TestActorRef
import com.github.golem.army.Commander
import com.github.golem.test.GolemActorUnitSpec
import com.github.golem.command.setup.SetBoardSize
import com.github.golem.command.game.MadeMove
import com.github.golem.model.{Pass, Human, Engine, Put}
import com.github.golem.model.Board.{Stone, Coords}
import akka.actor.{ActorRef, Identify}

class commander_test extends GolemActorUnitSpec {

/*  // TODO write more appropriate tests!
  "A commander" should "create create new spies and soldiers for new stones" in {
    val testActor = TestActorRef(new Commander, "commander")
    val commander = testActor.underlyingActor
    testActor ! SetBoardSize(9)
    testActor ! MadeMove(Put(Stone(Coords(2, 2), Human)))
    println(testActor.underlyingActor.getSubordinates) // FIXME This is awful! use logger!
    testActor ! MadeMove(Put(Stone(Coords(2, 3), Engine)))
    println(testActor.underlyingActor.getSubordinates) // FIXME This is awful! use logger!
    testActor ! MadeMove(Put(Stone(Coords(3,4), Human)))
    println(testActor.underlyingActor.getSubordinates)
  }*/

  /*"A commander" should "remove ghosts" in {
    val testActor = TestActorRef(new Commander, "commander2")
    testActor ! SetBoardSize(9)
    testActor ! MadeMove(Put(Stone(Coords(2, 2), Human)))
    println(testActor.underlyingActor.getSubordinates)
    testActor ! MadeMove(Put(Stone(Coords(2, 3), Engine)))
    println(testActor.underlyingActor.getSubordinates)
    testActor ! MadeMove(Put(Stone(Coords(3, 3), Human)))
    println(testActor.underlyingActor.getSubordinates)
    testActor ! MadeMove(Pass(Engine))
    println(testActor.underlyingActor.getSubordinates)
    testActor ! MadeMove(Put(Stone(Coords(1, 3), Human)))
    println(testActor.underlyingActor.getSubordinates)
    testActor ! MadeMove(Pass(Engine))
    println(testActor.underlyingActor.getSubordinates)
    testActor ! MadeMove(Put(Stone(Coords(2, 4), Human)))
    println(testActor.underlyingActor.getSubordinates)
  }
  */

  /*"A commander" should "only update existing chain only" in {
    val testActor = TestActorRef(new Commander, "commander3")
    testActor ! SetBoardSize(9)
    testActor ! MadeMove(Put(Stone(Coords(2,2), Human)))
    println(testActor.underlyingActor.getSubordinates)
    testActor ! MadeMove(Pass(Engine))
    testActor ! MadeMove(Put(Stone(Coords(2,3), Human)))
    println(testActor.underlyingActor.getSubordinates)
    testActor ! MadeMove(Pass(Engine))
    testActor ! MadeMove(Put(Stone(Coords(3,3), Human)))
    println(testActor.underlyingActor.getSubordinates)
    testActor ! MadeMove(Pass(Engine))
    testActor ! MadeMove(Put(Stone(Coords(3,2), Human)))
    println(testActor.underlyingActor.getSubordinates)
  }*/

  /*"A commander" should "join two existing chains" in {
    val testActor = TestActorRef(new Commander, "commander3")
    testActor ! SetBoardSize(9)
    testActor ! MadeMove(Put(Stone(Coords(2,2), Engine)))
    println(testActor.underlyingActor.getSubordinates)
    testActor ! MadeMove(Pass(Human))
    testActor ! MadeMove(Put(Stone(Coords(3,3), Engine)))
    println(testActor.underlyingActor.getSubordinates)
    testActor ! MadeMove(Pass(Human))
    testActor ! MadeMove(Put(Stone(Coords(2,3), Engine)))
    println(testActor.underlyingActor.getSubordinates)
  }*/

  "A commander" should "join two existing chains" in {
    val testActor = TestActorRef(new Commander, "commander3")
    testActor ! SetBoardSize(9)
    testActor ! MadeMove(Put(Stone(Coords(2,2), Engine)))
    println(testActor.underlyingActor.getSubordinates)
    testActor ! MadeMove(Pass(Human))
    testActor ! MadeMove(Put(Stone(Coords(3,3), Engine)))
    println(testActor.underlyingActor.getSubordinates)
    testActor ! MadeMove(Pass(Human))
    testActor ! MadeMove(Put(Stone(Coords(4,4), Engine)))
    println(testActor.underlyingActor.getSubordinates)
    testActor ! MadeMove(Pass(Human))
    testActor ! MadeMove(Put(Stone(Coords(2,3), Engine)))
    println(testActor.underlyingActor.getSubordinates)
  }

}
