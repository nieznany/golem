package com.github.golem.command.setup

/**
 * Starts new game.
 * TODO why board size is sent from KGS server 3 times?
 */
case class SetBoardSize(size : Int) extends SetupCommand

