package com.github.golem.command.tournament

import com.github.golem.model.Player
import com.github.golem.command.{Informative, Command}

case class TimeLeft(player: Player, leftSeconds: Int, leftStones: Int) extends Command with Informative

