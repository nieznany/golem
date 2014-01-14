package com.github.golem

import akka.actor.ActorSystem
import com.github.golem.client.kgs.KgsClient
import com.github.golem.command.administrative.StartClient

object Main {

  def main(args: Array[String]) {
    if(args.length < 2) {
      throw new IllegalArgumentException("You should provide exactly two parameters:" +
        " path to kgs client properties file, path to log file")
    }
    val system = ActorSystem("golem")
    val client = system.actorOf(KgsClient.props(args(0), args(1)), "kgsclient")
    client ! StartClient
  }

}
