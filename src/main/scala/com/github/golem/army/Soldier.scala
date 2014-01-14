package com.github.golem.army

import akka.actor.{Props, Actor}
import akka.event.Logging

object Soldier {
  def name = "soldier"
  def props = Props(classOf[Soldier])
}

class Soldier extends Actor {
  import context._
  val LOG = Logging(system, this)

  def receive: Actor.Receive = {
    case x => LOG.warning(s"UFO $x")
  }
}
