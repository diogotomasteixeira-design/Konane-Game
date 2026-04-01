object Types {

  import "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0"

  type Coord2D = (Int, Int)
  type Board = ParMap2[Coord2D, Stone]

  enum Stone:
    case Black, White
}
