import scala.collection.parallel.immutable.ParMap

object Types {

  type Coord2D = (Int, Int) //(row, column)
  type Board = ParMap[Coord2D, Stone]

  enum Stone:
    case Black, White
}
