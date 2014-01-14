package com.github.golem.army

import akka.actor.{Props, Actor}

object Spy {
  def name = "spy"
  def props = Props(classOf[Spy])
}

class Spy extends Actor {
  def receive: Actor.Receive = {
    case _ => ???
  }
}
