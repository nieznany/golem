package com.github.golem.client.kgs.command

import com.github.golem.command.administrative.AdministrativeCommand
import com.github.golem.command.CommandResponse

object ListCommands {
  case class Response(commandsName: List[String]) extends CommandResponse
}

class ListCommands extends AdministrativeCommand
