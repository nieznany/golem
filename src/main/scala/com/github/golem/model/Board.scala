package com.github.golem.model


import com.github.golem.model.Board._
import com.github.golem.model.BasicRulesGame.{Group, Chain, BoardDecomposition}

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

  //FIXME trait never used
  trait Empty

  case class Border(override val position: Coords) extends Field with Disabled

  case class Free(override val position: Coords) extends FreeField

  case class Stone(override val position: Coords, owner: Player) extends Field with Disabled {
    def toFree = Free(position)
  }

  case class Unavailable(override val position: Coords, disabledFor: Player) extends FreeField with Disabled {
    def this(freeField: FreeField, player: Player) = {
      this(freeField.position, player)
    }
  }


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
            case 'O' => Unavailable(Coords(i, j), Human)
            case 'X' => Unavailable(Coords(i, j), Engine)
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
 * Numerated from 1 to nrows and from 1 to ncolumns (inclusive in both)
 */
case class Board private(nrows: Int, ncolumns: Int, fields: Map[Coords, Field]) {
  private var decomposition: Option[BoardDecomposition] = None/*TODO is it good place to store board decomposition?*/
  def setDecomposition(d: BoardDecomposition) = this.decomposition = Some(d)
  def getDecomposition = this.decomposition
  def getDecomposedNonEmptyChain(coords: Coords): Chain = {
    this.decomposition match {
      case Some(decomposition) => {
        decomposition.chainMap(coords)
      }
      case None => throw new Exception("Board is not decomposed.")
    }
  }

  def getDecomposedGroup(coords: Coords): Option[Group] = {
    this.decomposition match {
      case Some(decomposition) => {
        decomposition.groupMap.get(coords)
      }
      case None => throw new Exception("Board is not decomposed.")
    }
  }

  def getDecomposedNonEmptyGroup(coords: Coords):Group = {
    this.decomposition match {
      case Some(decomposition) => {
        decomposition.groupMap(coords)
      }
      case None => throw new Exception("Board is not decomposed.")
    }
  }

  /**
   * Tests wheter given coordinates are out of bounds (including border)
   */
  def isOutOfBounds(row: Int, column: Int) = {
    row < 0 || row > nrows + 1 || column < 0 || column > ncolumns + 1
  }

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

  /**
   * Returns true, if selected field is of type Disabled, otherwise false.
   */
  def isDisabled(coords: Coords): Boolean = this(coords).isInstanceOf[Disabled]

  def +(field: Field): Board = this.copy(fields = this.fields + (field.position -> field))

  /**
   * Will update field, if already exists.
   */
  def ++(newFields: Iterable[Field]): Board = this.copy(fields = this.fields ++ (for {field <- newFields} yield field.position -> field))

  def getFreeFields: Set[Field] = {
    val ff = scala.collection.mutable.Set[Field]()
    for(i <- 1 to nrows) {
      for(j <- 1 to ncolumns) {
        this(i, j) match {
          case f: Free => ff += f
          case _ => {}
        }
      }
    }
    ff.toSet
  }

  override def toString = {
    val result = new StringBuilder(s"board($nrows x $ncolumns):\n")
    for(i <- 1 to nrows) {
      for (j <- 1 to ncolumns) {
        val c = this(i, j) match {
          case Stone(_, Engine) => 'x'
          case Stone(_, Human) => 'o'
          case Unavailable(_, Human) => 'O'
          case Unavailable(_, Engine) => 'X'
          case _: FreeField => '.'
        }
        result append c
      }
      result append '\n'
    }
    result.toString
  }
  fields(Coords(1,0))
}
