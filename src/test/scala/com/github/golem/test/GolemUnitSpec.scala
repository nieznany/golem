package com.github.golem.test

import org.scalatest._
import akka.testkit.TestKit
import akka.actor.ActorSystem

abstract class GolemUnitSpec extends FlatSpec
                             with Matchers
                             with OptionValues
                             with Inside
                             with Inspectors

abstract class GolemActorUnitSpec extends TestKit(ActorSystem("test"))
                                  with FlatSpecLike
                                  with Matchers
                                  with OptionValues
                                  with Inside
                                  with Inspectors