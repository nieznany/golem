package com.github.golem.command.administrative

import com.github.golem.command.CommandResponse

object GetName extends AdministrativeCommand {
  case class Response(name: String) extends CommandResponse
}
