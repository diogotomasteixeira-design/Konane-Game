import Types.{Board, Coord2D, Stone}

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParMap

case class Game(board: Board, lstOpenCoords: List[Coord2D], player: Stone, rows: Int, cols: Int) {

  def randomMove(rand: MyRandom): (Coord2D, MyRandom) = {
    Game.randomMove(this.lstOpenCoords, rand)
  }

  def play(coordFrom: Coord2D, coordTo: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D], Board) = {
    Game.play(this.board, this.player, coordFrom, coordTo, this.lstOpenCoords)
  }

  def playRandomly(r: MyRandom): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) = {
    Game.playRandomly(this.board, r, this.player, this.lstOpenCoords, Game.randomMove)
  }
}

object Game {

  def initBoard(rows: Int, cols: Int): (Board, List[Coord2D]) = {
    def generateCoords(rows: Int, cols: Int): List[Coord2D] = {
      @tailrec
      def loop(r: Int, c: Int, acc: List[Coord2D]): List[Coord2D] = {
        if (r >= rows) acc.reverse
        else if (c >= cols) loop(r + 1, 0, acc)
        else loop(r, c + 1, (r, c) :: acc)
      }
      loop(0, 0, Nil)
    }

    val coords = generateCoords(rows, cols)

    val fullBoard = coords.foldLeft(ParMap.empty[Coord2D, Stone]) { (acc, coord) =>
      val (r, c) = coord
      val stone = if (r + c) % 2 == 0 then Stone.Black else Stone.White
      acc + (coord -> stone)
    }

    val centerBlack = (rows / 2, cols / 2)
    val centerWhite = (rows / 2, cols / 2 - 1)
    val board = fullBoard - centerBlack - centerWhite

    val openCoords = List(centerBlack, centerWhite)
    (board, openCoords)
  }

  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val (n, nextRandom) = rand.nextInt
    val coord = math.abs(n) % lstOpenCoords.length
    (lstOpenCoords(coord), nextRandom)
  }

  def play(board: Board, player: Stone, coordFrom: Coord2D, coordTo: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D], Board) = {
    if (validMove(board, player, coordFrom, coordTo, lstOpenCoords)) {
      val captured = ((coordFrom._1 + coordTo._1) / 2, (coordFrom._2 + coordTo._2) / 2)
      val newBoard = board - coordFrom - captured + (coordTo -> player)
      val newOpenCoords = lstOpenCoords.filterNot(_ == coordTo) :+ coordFrom :+ captured
      (Some(newBoard), newOpenCoords, board)
    } else {
      (None, lstOpenCoords, board)
    }
  }

  def validMove(board: Board, player: Stone, coordFrom: Coord2D, coordTo: Coord2D, lstOpenCoords: List[Coord2D]): Boolean = {
    val captured = ((coordFrom._1 + coordTo._1) / 2, (coordFrom._2 + coordTo._2) / 2)
    val cap: Option[Stone] = board.get(captured)

    val capturedIsEnemy = cap match {
      case Some(Stone.Black) =>
        player == Stone.White
      case Some(Stone.White) =>
        player == Stone.Black
      case None =>
        false
    }
    val res: Boolean = capturedIsEnemy && lstOpenCoords.contains(coordTo) && board.get(coordFrom).contains(player)
    res
  }


  def validMoveExists(board: Board, player: Stone, coordFrom: Coord2D, lstOpenCoords: List[Coord2D]): Boolean = {
    val (r, c) = coordFrom
    val possibleDestinations = List((r - 2, c), (r + 2, c), (r, c - 2), (r, c + 2))
    possibleDestinations.exists(coordTo => validMove(board, player, coordFrom, coordTo, lstOpenCoords))
  }

  def hasValidMoves(board: Board, player: Stone, lstOpenCoords: List[Coord2D]): Boolean = {
    board.exists { case (coord, stone) =>
      stone == player && validMoveExists(board, player, coord, lstOpenCoords)
    }
  }

  @tailrec
  def playRandomly(board: Board, r: MyRandom, player: Stone, lstOpenCoords: List[Coord2D], f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) = {
    val (coordTo,newRandom) = f(lstOpenCoords, r)
    findPieceForMove(board, player, coordTo, lstOpenCoords) match {
      case Some(coordFrom) =>
        val (newBoard, newLstOpenCoords, _) = play(board, player, coordFrom, coordTo, lstOpenCoords)
        (newBoard, newRandom, newLstOpenCoords, Some(coordTo))
      case None => playRandomly(board, newRandom, player, lstOpenCoords, f)  
    }
  }

  def findPieceForMove(board: Board, player: Stone, coordTo: Coord2D, lstOpenCoords: List[Coord2D]): Option[Coord2D] = {
    val possibleFroms = List(
      (coordTo._1 - 2, coordTo._2),
      (coordTo._1 + 2, coordTo._2),
      (coordTo._1, coordTo._2 - 2),
      (coordTo._1, coordTo._2 + 2)
    )
    possibleFroms.find(from =>
      board.get(from).contains(player) && validMove(board, player, from, coordTo, lstOpenCoords)
    )
  }
  def canKeepJumping(board: Board, player: Stone, currentCoord: Coord2D, lstOpenCoords: List[Coord2D]): Boolean = {
    validMoveExists(board, player, currentCoord, lstOpenCoords)
  }
}