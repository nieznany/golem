package com.github.golem.command.game

import com.github.golem.model.{Move, Put}
import com.github.golem.command.{Informative, Command}

case class MadeMove(move: Move) extends Command with Informative
