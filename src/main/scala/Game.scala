import Types.{Board, Coord2D, Stone}

import scala.collection.parallel.immutable.ParMap

object Game {

  def initBoard(rows: Int, cols: Int): Board ={
    val coords = for
      r <- 0 until rows
      c <- 0 until cols
    yield (r, c)
  
    coords.foldLeft(ParMap.empty[Coord2D, Stone]) { (acc, coord) =>
      val (r, c) = coord
      val stone = if (r + c) % 2 == 0 then Stone.Black else Stone.White
      acc + (coord -> stone)
    }

  }

  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val (n, nextRandom) = rand.nextInt
    val coord = math.abs(n) % lstOpenCoords.length
    (lstOpenCoords(coord), nextRandom)
  }

}
