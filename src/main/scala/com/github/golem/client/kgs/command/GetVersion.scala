package com.github.golem.client.kgs.command

import com.github.golem.command.administrative.AdministrativeCommand
import com.github.golem.command.CommandResponse

object GetVersion {
  case class Response(version: String) extends CommandResponse
}

class GetVersion extends AdministrativeCommand
