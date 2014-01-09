package com.github.golem.command.administrative

import com.github.golem.command.CommandResponse

object ListCommands extends AdministrativeCommand {
  case class Response(commandsName: List[String]) extends CommandResponse
}

