package com.github.golem.army

import akka.actor.Actor

class Spy extends Actor {
  def receive: Actor.Receive = {
    case _ => ???
  }
}
