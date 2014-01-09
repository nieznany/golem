package com.github.golem.command.administrative

import com.github.golem.command.CommandResponse

object GetVersion extends AdministrativeCommand {
  case class Response(version: String) extends CommandResponse
}

