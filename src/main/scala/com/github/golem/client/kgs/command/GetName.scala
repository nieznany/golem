package com.github.golem.client.kgs.command

import com.github.golem.command.administrative.AdministrativeCommand
import com.github.golem.command.CommandResponse

object GetName {
  case class Response(name: String) extends CommandResponse
}

class GetName extends AdministrativeCommand
