package com.github.golem.army

import akka.actor.Actor
import akka.event.LoggingAdapter

// TODO use actor's strategies instead of this class!
abstract class GolemActor extends Actor {
  def receive: Actor.Receive = {
    case message => {
      try {
        getLogger.info(s"Received message: $message")
        handle(message)
      } catch {
        case t: Throwable => getLogger.error(t, "Unhandled error")
      }
    }
  }

  def getLogger: LoggingAdapter
  def handle(message: Any)
}
