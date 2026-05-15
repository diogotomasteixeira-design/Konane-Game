package konane

import konane.Types.Stone
import konane.Types.Stone.*



class GameAPI(rows: Int, cols: Int) {

  private val (board, openCoords) =
    Game.initBoard(rows, cols)

  private var game =
    Game(board, openCoords, Black, rows, cols, difficulty = )

  private var selected: Option[(Int, Int)] = None

  def getRows: Int =
    game.rows

  def getCols: Int =
    game.cols

  def getStone(row: Int, col: Int): String = {

    game.board.get((row, col)) match {

      case Some(Black) => "BLACK"

      case Some(White) => "WHITE"

      case None => "EMPTY"
    }
  }

  def isSelected(row: Int, col: Int): Boolean = {
    selected.contains((row, col))
  }

  def currentPlayer: String = {

    game.player match {

      case Black => "BLACK"

      case White => "WHITE"
    }
  }

  def select(row: Int, col: Int): Boolean = {

    selected match {

      case None =>

        game.board.get((row, col)) match {

          case Some(stone)
            if stone == game.player =>

            selected = Some((row, col))
            true

          case _ =>
            false
        }

      case Some(from) =>

        val to = (row, col)

        val (newBoard, newOpen) =
          game.play(from, to, game.lstOpenCoords)

        newBoard match {

          case Some(board) =>

            game = Game(
              board,
              newOpen,
              Game.nextPlayer(game.player),
              game.rows,
              game.cols
            )

            selected = None

            true

          case None =>

            selected = None

            false
        }
    }
  }
}