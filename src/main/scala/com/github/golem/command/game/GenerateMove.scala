package com.github.golem.command.game

import com.github.golem.command.{CommandResponse, Command}
import com.github.golem.model.Move

object GenerateMove extends Command {
  case class Response(move: Move) extends CommandResponse
}
