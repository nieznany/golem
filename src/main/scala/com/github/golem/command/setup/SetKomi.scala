package com.github.golem.command.setup

import com.github.golem.command.{Informative, Command}

case class SetKomi(newKomi: Double) extends Command with Informative
