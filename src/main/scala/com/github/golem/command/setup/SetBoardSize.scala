package com.github.golem.command.setup

import com.github.golem.command.CommandResponse

object SetBoardSize {
  class Response extends CommandResponse
}

/**
 * Starts new game.
 */
case class SetBoardSize(size : Int) extends SetupCommand

