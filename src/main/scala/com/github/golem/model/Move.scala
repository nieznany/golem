package com.github.golem.model

import com.github.golem.model.Move.Coordinates

object Move {

  case class Coordinates(x: Int, y: Int) // TODO Actually, this is a tuple - Coordinates should be an alias for (Int, Int)

}

/**
 * @param opponent true, when move was made by opponent (for engine).
 */
case class Move(position: Coordinates, opponent: Boolean)

class Pass(opponent: Boolean) extends Move(null, opponent)
