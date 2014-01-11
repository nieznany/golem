package com.github.golem.model


import com.github.golem.model.Board._

object Board {
  val N = Coords(-1, 0)
  val S = Coords(1, 0)
  val W = Coords(0, -1)
  val E = Coords(0, 1)

  case class Coords(row: Int, column: Int) extends {
    // TODO Actually, this is a tuple  - Coordinates could be an alias for (Int, Int), but ._1,._2 have no meaning!
    def +(c: Coords): Coords = Coords(row + c.row, column + c.column)
  }

  trait Field {
    def position: Coords
  }

  trait FreeField extends Field

  trait Disabled

  trait Empty

  case class Border(override val position: Coords) extends Field with Disabled

  case class Free(override val position: Coords) extends FreeField

  case class Stone(override val position: Coords, owner: Player) extends Field with Disabled

  case class Ko(override val position: Coords, disabledFor: Player) extends FreeField with Disabled


  def apply(nrows: Int, ncolumns: Int) = {
    val fields = scala.collection.mutable.Map[Coords, Field]()
    for (i <- 0 to ncolumns) {
      // border rows
      fields += (Coords(0, i) -> Border(Coords(0, i)))
      fields += (Coords(nrows + 1, i) -> Border(Coords(nrows + 1, i)))
      // border collumns
      fields += (Coords(i, 0) -> Border(Coords(i, 0)))
      fields += (Coords(i, ncolumns + 1) -> Border(Coords(ncolumns + 1, i)))
    }
    new Board(nrows, ncolumns, fields.toMap[Coords, Field])
  }

  def apply(rows: Seq[String]): Board = {
    val nrows = rows.length
    val ncolumns:Int = rows.reduceLeft((a:String, b:String) => if (a.length > b.length) a else b).length
    var fields = List[Field]()
    var i = 1
    for (row <- rows) {
      var j = 1
      fields ++= (
        for (c <- row) yield {
          val field = c match {
            case 'o' => Stone(Coords(i, j), Human)
            case 'x' => Stone(Coords(i, j), Engine)
            case '.' => Free(Coords(i, j))
          }
          j += 1
          field
        })
      i += 1
    }
    Board(nrows, ncolumns) ++ fields
  }
}

/**
 * Must be immutable.
 * Numerated from 1 to nrows and from 1 to ncolumns (inclusive in both)
 */
case class Board private(nrows: Int, ncolumns: Int, fields: Map[Coords, Field]) {

  /**
   * Tests wheter given coordinates are out of bounds (including border)
   */
  def isOutOfBounds(row: Int, column: Int) = {
    row < 0 || row > nrows + 1 || column < 0 || column > ncolumns + 1
  }
  //  def isLegal(m: Put): Boolean = { // Should not be here! Game state should contains method to check, if given field is illegal for given player
  //    val r = m.stone.position.row
  //    val c = m.stone.position.column
  //    ! isOutOfBounds(r, c) && this(r, c) != FieldState.FREE
  //  }

  @throws[IndexOutOfBoundsException]("Field is out of bounds.")
  def apply(row: Int, column: Int): Field = {
    if (isOutOfBounds(row, column)) {
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
  def ++(newFields: Iterable[Field]): Board = this.copy(fields = this.fields ++ (for {field <- newFields} yield field.position -> field))

  override def toString = {
    val result = new StringBuilder(s"board($nrows x $ncolumns):\n")
    for(i <- 0 to nrows + 1) {
      for (j <- 0 to ncolumns + 1) {
        val c = this(i, j) match {
          case Stone(_, Engine) => 'x'
          case Stone(_, Human) => 'o'
          case _: FreeField => '.'
        }
        result append c
      }
      result append '\n'
    }
    result.toString
  }
}
