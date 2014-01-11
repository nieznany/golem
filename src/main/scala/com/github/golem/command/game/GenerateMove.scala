package com.github.golem.command.game

import com.github.golem.model.Put
import com.github.golem.command.Command

case class GenerateMove(move: Put) extends Command
