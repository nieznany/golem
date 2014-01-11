package com.github.golem.model

import scala.collection.immutable.HashMap

import com.github.golem.model.Board._

object Board {

  case class Coords(row: Int, column: Int) extends {
    // TODO Actually, this is a tuple  - Coordinates could be an alias for (Int, Int), but ._1,._2 have no meaning!
    def +(c: Coords): Coords = Coords(row + c.row, column + c.column)
  }

  trait Field {
    def position: Coords
  }

  trait Disabled

  trait Empty

  case class Border(override val position: Coords) extends Field with Disabled

  case class Free(override val position: Coords) extends Field with Empty

  case class Stone(override val position: Coords, owner: Player) extends Field with Disabled

  case class Ko(override val position: Coords, disabledFor: Player) extends Field with Empty with Disabled


  def apply(nrows: Int, ncolumns: Int) = {
    Board(nrows, ncolumns, HashMap[Coords, Field]())
  }
}

/**
 * Must be immutable.
 */
case class Board private(nrows: Int, ncolumns: Int, fields: Map[Coords, Field]) {
  val N = Coords(-1, 0)
  val S = Coords(1, 0)
  val W = Coords(0, -1)
  val E = Coords(0, 1)

  def isOutOfBounds(row: Int, column: Int) =  row < 1 || row > nrows || column < 1 || column > ncolumns

  def isLegal(m: Put): Boolean = { // Should not be here! Game state should contains method to check, if given field is illegal for given player
    val r = m.stone.position.row
    val c = m.stone.position.column
    ! isOutOfBounds(r, c) && this(r, c) != FieldState.FREE
  }

  @throws[IndexOutOfBoundsException]("Field is out of bounds.")
  def apply(row: Int, column: Int): Field = {
    if(isOutOfBounds(row, column)) {
      throw new IndexOutOfBoundsException(s"row: $row, column: $column")
    }
    fields.get(Coords(row, column)) match {
      case None => Free(Coords(row, column))
      case Some(value) => value
    }
  }

  def apply(coords: Coords): Field = apply(coords.row, coords.column)

  def +(field: Field): Board = this.copy(fields = this.fields + (field.position -> field))

  /**
   * Will update field, if already exists.
   */
  def +(newFields: Iterable[Field]): Board = this.copy(fields = this.fields ++ (for {field <- newFields} yield field.position -> field))

}
