package konane

import konane.Types.Stone
import konane.Types.Stone.*
import konane.Types.Coord2D

class GameAPI(rows: Int, cols: Int, difficulty: Int) {

  private val (initialBoard, initialOpen) = Game.initBoard(rows, cols)
  private var game = Game(initialBoard, initialOpen, Black, rows, cols, difficulty)

  private var selected: Option[Coord2D] = None
  private var multiJumpPiece: Option[Coord2D] = None
  private var rand = MyRandom.create()
  private var history: List[Game] = Nil

  def getRows: Int = game.rows
  def getCols: Int = game.cols
  def getDifficulty: Int = game.difficulty

  def getStone(row: Int, col: Int): String = {
    game.board.get((row, col)) match {
      case Some(Black) => "BLACK"
      case Some(White) => "WHITE"
      case None        => "EMPTY"
    }
  }

  def isSelected(row: Int, col: Int): Boolean = {
    selected.contains((row, col)) || multiJumpPiece.contains((row, col))
  }

  def currentPlayer: String = if (game.player == Black) "BLACK" else "WHITE"

  def nextPlayer(player: Stone): Stone = if (player == Black) White else Black

  def isGameOver: Boolean = {
    if (game.player == Black && multiJumpPiece.isDefined) false
    else !Game.hasValidMoves(game.board, game.player, game.lstOpenCoords)
  }

  def jogarAleatorioHumano(): Unit = {
    if (game.player != Black || isGameOver) {
      return
    }
    history = game :: history

    val (optBoard, novoRand, novaLista, optFrom, optTo) = Game.playRandomly(
      game.board, rand, Black, game.lstOpenCoords, game.difficulty, Game.randomMove
    )

    rand = novoRand

    optBoard match {
      case Some(board) =>
        game = Game(board, novaLista, White, game.rows, game.cols, game.difficulty)
        selected = None
        multiJumpPiece = None
      case None =>
        game = Game(game.board, game.lstOpenCoords, White, game.rows, game.cols, game.difficulty)
        selected = None
        multiJumpPiece = None
    }
  }

  def select(row: Int, col: Int, currentDiff: Int): Boolean = {
    if (game.player != Black || isGameOver) {
      return false
    }

    val clickedCoord = (row, col)

    multiJumpPiece match {
      case Some(fromPiece) =>
        if (Game.validMove(game.board, game.player, fromPiece, clickedCoord, game.lstOpenCoords)) {
          val (optBoard, newOpen, _) = game.play(fromPiece, clickedCoord, game.lstOpenCoords)

          optBoard match {
            case Some(board) =>
              if (Game.validMoveExists(board, game.player, clickedCoord, newOpen)) {
                game = Game(board, newOpen, game.player, game.rows, game.cols, currentDiff)
                multiJumpPiece = Some(clickedCoord)
              } else {
                game = Game(board, newOpen, White, game.rows, game.cols, currentDiff)
                multiJumpPiece = None
              }
              selected = None
              true
            case None => false
          }
        } else {
          false
        }

      case None =>
        selected match {
          case None =>
            game.board.get(clickedCoord) match {
              case Some(stone) if stone == game.player =>
                selected = Some(clickedCoord)
                true
              case _ => false
            }

          case Some(from) =>
            if (Game.validMove(game.board, game.player, from, clickedCoord, game.lstOpenCoords)) {
              val (optBoard, newOpen, _) = game.play(from, clickedCoord, game.lstOpenCoords)

              optBoard match {
                case Some(board) =>
                  history = game :: history
                  if (Game.validMoveExists(board, game.player, clickedCoord, newOpen)) {
                    game = Game(board, newOpen, game.player, game.rows, game.cols, currentDiff)
                    multiJumpPiece = Some(clickedCoord)
                  } else {
                    game = Game(board, newOpen, White, game.rows, game.cols, currentDiff)
                    multiJumpPiece = None
                  }
                  selected = None
                  true
                case None =>
                  selected = None
                  false
              }
            } else {
              selected = None
              false
            }
        }
    }
  }

  def stopCapturing(): Unit = {
    if (game.player == Black && multiJumpPiece.isDefined) {
      game = Game(game.board, game.lstOpenCoords, White, game.rows, game.cols, game.difficulty)
      multiJumpPiece = None
      selected = None
    }
  }

  def isMultiJumping: Boolean = multiJumpPiece.isDefined

  def undo(): Boolean = {
    history match {
      case head :: tail =>
        game = head
        history = tail
        selected = None
        multiJumpPiece = None
        true
      case Nil =>
        false
    }
  }

  def reset(): Unit = {
    val (board, open) = Game.initBoard(rows, cols)
    game = Game(board, open, Black, rows, cols, difficulty)
    selected = None
    multiJumpPiece = None
    history = Nil
  }

  def jogarComputador(): Unit = {
    if (game.player != White || isGameOver) {
      return
    }

    var continuarLoop = true
    var pecaAtualIA: Option[Coord2D] = None

    while (continuarLoop && Game.hasValidMoves(game.board, White, game.lstOpenCoords)) {
      pecaAtualIA match {
        case None =>
          val (optBoard, novoRand, novaLista, optFrom, optTo) = if (game.difficulty == 2) {
            val (nb, no, cFrom, cTo) = Game.playMaxJumps(game.board, White, game.lstOpenCoords)
            (nb, rand, no, cFrom, cTo)
          } else {
            val (nb, r, no, cFrom, cTo) = Game.playRandomly(game.board, rand, White, game.lstOpenCoords, game.difficulty, Game.randomMove)
            (nb, r, no, cFrom, cTo)
          }

          rand = novoRand

          optBoard match {
            case Some(board) if optFrom.isDefined && optTo.isDefined =>
              val destino = optTo.get
              if (Game.validMoveExists(board, White, destino, novaLista)) {
                game = Game(board, novaLista, White, game.rows, game.cols, game.difficulty)
                pecaAtualIA = Some(destino)
              } else {
                game = Game(board, novaLista, Black, game.rows, game.cols, game.difficulty)
                continuarLoop = false
              }
            case _ =>
              continuarLoop = false
          }

        case Some(fromPiece) =>
          val destinations = List(
            (fromPiece._1 - 2, fromPiece._2), (fromPiece._1 + 2, fromPiece._2),
            (fromPiece._1, fromPiece._2 - 2), (fromPiece._1, fromPiece._2 + 2)
          ).filter(to => Game.validMove(game.board, White, fromPiece, to, game.lstOpenCoords))

          if (destinations.isEmpty) {
            game = Game(game.board, game.lstOpenCoords, Black, game.rows, game.cols, game.difficulty)
            continuarLoop = false
          } else {
            val proximoDestino = if (game.difficulty == 2) {
              destinations.maxBy(to => {
                val (nb, nl, _) = Game.play(game.board, White, fromPiece, to, game.lstOpenCoords)
                Game.maxJumpsFrom(nb.get, White, to, nl)
              })
            } else {
              val (n, nextRand) = rand.nextInt
              rand = nextRand
              destinations(math.abs(n) % destinations.length)
            }

            val (optBoard, novaLista, _) = Game.play(game.board, White, fromPiece, proximoDestino, game.lstOpenCoords)

            optBoard match {
              case Some(board) =>
                if (Game.validMoveExists(board, White, proximoDestino, novaLista)) {
                  game = Game(board, novaLista, White, game.rows, game.cols, game.difficulty)
                  pecaAtualIA = Some(proximoDestino)
                } else {
                  game = Game(board, novaLista, Black, game.rows, game.cols, game.difficulty)
                  continuarLoop = false
                }
              case None =>
                continuarLoop = false
            }
          }
      }
    }

    if (game.player == White) {
      game = Game(game.board, game.lstOpenCoords, Black, game.rows, game.cols, game.difficulty)
    }
  }
}