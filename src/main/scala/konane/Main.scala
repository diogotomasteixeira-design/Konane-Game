package konane

import konane.Types.Stone.{Black, White}
import konane.Types.{Board, Coord2D, Stone}

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Main {

  def main(args: Array[String]): Unit = {
    println("=== Bem-vindo ao Kōnane ===")
    println("Tu jogas com as Pretas (B). O computador joga com as Brancas (W).")
    println()

    val (rows, cols, limitSeconds, difficulty) = TUI.mainMenu()
    val (board, lstOpenCoords) = Game.initBoard(rows, cols)
    val initialGame = Game(board, lstOpenCoords, Black, rows, cols, difficulty)
    val rand = MyRandom.create()
    val history = Nil

    TUI.printGameState(initialGame)
    gameLoop(initialGame, rand, history, limitSeconds, difficulty)
  }

  @tailrec
  def gameLoop(game: Game, rand: MyRandom, history: List[Game], limitSeconds: Long, difficulty: Integer): Unit = {

    if (!Game.hasValidMoves(game.board, game.player, game.lstOpenCoords)) {
      val vencedor = if (game.player == Black) "O computador (Brancas) ganhou!" else "Tu (Pretas) ganhaste!"
      println(s"\n=== Fim de jogo! $vencedor ===")
    } else {
      game.player match {
        case Black =>
          println("A tua vez! Escolhe uma opção:")
          println("  1 - Introduzir coordenada de destino manualmente")
          println("  2 - Jogada aleatória")
          println("  3 - Undo (Anular última jogada)")
          println("  4 - Reiniciar jogo")

          val opcao = readLine("Opção: ").trim

          opcao match {
            case "1" =>
              handleHumanMove(game, rand, history, limitSeconds, difficulty)

            case "2" =>
              println("A jogar aleatoriamente...")
              val (optBoard, nextRand, nextOpen, optCoordFrom, optCoordTo) =
                Game.playRandomly(game.board, rand, game.player, game.lstOpenCoords, difficulty, Game.randomMove)

              optBoard match {
                case Some(newBoard) =>
                  println(s"Jogaste para: ${coordToStr(optCoordTo.get)}")
                  val nextGame = Game(newBoard, nextOpen, White, game.rows, game.cols, difficulty)
                  TUI.printGameState(nextGame)
                  gameLoop(nextGame, nextRand, game :: history, limitSeconds, difficulty)
                case None =>
                  println("\n=== Não existem mais movimentos! O computador (Brancas) ganhou! ===")
              }

            case "3" =>
              history match {
                case Nil =>
                  println("Não há mais jogadas para anular!")
                  gameLoop(game, rand, history, limitSeconds, difficulty)
                case previousState :: olderHistory =>
                  println("Undo realizado com sucesso!")
                  TUI.printGameState(previousState)
                  gameLoop(previousState, rand, olderHistory, limitSeconds, difficulty)
              }

            case "4" =>
              val (rows, cols, limitSeconds, difficulty) = TUI.mainMenu()
              val (board, lstOpenCoords) = Game.initBoard(rows, cols)
              val initialGame = Game(board, lstOpenCoords, Black, rows, cols, difficulty)
              val rand = MyRandom.create()
              val history = Nil

              TUI.printGameState(initialGame)
              gameLoop(initialGame, rand, history, limitSeconds, difficulty)

            case _ =>
              println("Opção inválida, tenta novamente.")
              gameLoop(game, rand, history, limitSeconds, difficulty)
          }

        case White =>
          println("\nVez do computador (Brancas)...")

          val (optBoard, nextOpen, optCoordFrom, optCoordTo, nextRand) = if (difficulty == 2) {
            val (newBoard, newOpenList, coordFrom, coordTo) = Game.playMaxJumps(game.board, game.player, game.lstOpenCoords)
            (newBoard, newOpenList, coordFrom, coordTo, rand)
          } else {
            val (newBoard, r, newOpenList, coordFrom, coordTo) = Game.playRandomly(game.board, rand, game.player, game.lstOpenCoords, difficulty, Game.randomMove)
            (newBoard, newOpenList, coordFrom, coordTo, r)
          }

          optBoard match {
            case Some(newBoard) =>
              println(s"O computador jogou para: ${coordToStr(optCoordTo.get)}")

              val (finalBoard, finalOpen, _) = Game.play(newBoard, White, optCoordFrom.get, optCoordTo.get, nextOpen)

              val nextGame = Game(finalBoard.get, finalOpen, Black, game.rows, game.cols, difficulty)
              TUI.printGameState(nextGame)
              gameLoop(nextGame, nextRand, history, limitSeconds, difficulty)

            case None =>
              println("\n=== Não existem mais movimentos! Tu (Pretas) ganhaste! ===")
          }
      }
    }
  }

  @tailrec
  def handleHumanMove(game: Game, rand: MyRandom, history: List[Game], limitSeconds: Long, difficulty: Integer): Unit = {
    println(s"Tens $limitSeconds segundos para jogar!")
    val startTime = System.currentTimeMillis()

    println("Introduz a coordenada de destino (ex: B3 ou b3):")
    val input = readLine("Destino: ").trim

    val endTime = System.currentTimeMillis()
    val elapsedSeconds = (endTime - startTime) / 1000

    if (elapsedSeconds > limitSeconds) {
      println(s"Tempo esgotado! Demoraste $elapsedSeconds segundos.")
      gameLoop(game.copy(player = White), rand, history, limitSeconds, difficulty)
    } else {
      parseCoord(input, game.rows, game.cols) match {
        case None =>
          println("Coordenada inválida. Tenta novamente.")
          handleHumanMove(game, rand, history, limitSeconds, difficulty)

        case Some(coordTo) =>
          Game.findPieceForMove(game.board, game.player, coordTo, game.lstOpenCoords) match {
            case None =>
              println(s"Não existe nenhuma peça tua que possa mover para ${coordToStr(coordTo)}.")
              handleHumanMove(game, rand, history, limitSeconds, difficulty)

            case Some(coordFrom) =>
              val (optBoard, newOpen, _) = Game.play(game.board, game.player, coordFrom, coordTo, game.lstOpenCoords)
              optBoard match {
                case Some(newBoard) =>
                  println(s"Moveste de ${coordToStr(coordFrom)} para ${coordToStr(coordTo)}.")

                  if (Game.validMoveExists(newBoard, game.player, coordTo, newOpen)) {
                    val nextGame = Game(newBoard, newOpen, game.player, game.rows, game.cols, difficulty)
                    TUI.printGameState(nextGame)
                    handleMultiJump(nextGame, rand, game :: history, coordTo, limitSeconds, difficulty)
                  } else {
                    val nextGame = Game(newBoard, newOpen, White, game.rows, game.cols, difficulty)
                    TUI.printGameState(nextGame)
                    gameLoop(nextGame, rand, game :: history, limitSeconds, difficulty)
                  }

                case None =>
                  println("Jogada inválida. Tenta novamente.")
                  handleHumanMove(game, rand, history, limitSeconds, difficulty)
              }
          }
      }
    }
  }

  @tailrec
  def handleMultiJump(game: Game, rand: MyRandom, history: List[Game], pieceCoord: Coord2D, limitSeconds: Long, difficulty: Integer): Unit = {
    println(s"A peça em ${coordToStr(pieceCoord)} pode continuar a saltar!")
    println("Opções: 1 - Continuar a saltar | 2 - Parar captura (Passar vez)")

    val opcao = readLine("Escolha: ").trim

    opcao match {
      case "2" =>
        gameLoop(game.copy(player = White), rand, history, limitSeconds, difficulty)

      case "1" =>
        println(s"Introduz o próximo destino para a peça em ${coordToStr(pieceCoord)}:")
        val input = readLine("Novo Destino: ").trim

        parseCoord(input, game.rows, game.cols) match {
          case Some(newDest) if Game.validMove(game.board, game.player, pieceCoord, newDest, game.lstOpenCoords) =>
            val (optBoard, newOpen, _) = Game.play(game.board, game.player, pieceCoord, newDest, game.lstOpenCoords)
            val nextBoard = optBoard.get

            println(s"Saltaste para ${coordToStr(newDest)}.")

            if (Game.validMoveExists(nextBoard, game.player, newDest, newOpen)) {
              val nextGame = Game(nextBoard, newOpen, game.player, game.rows, game.cols, difficulty)
              TUI.printGameState(nextGame)
              handleMultiJump(nextGame, rand, history, newDest, limitSeconds, difficulty)
            } else {
              println("Não há mais saltos possíveis.")
              val nextGame = Game(nextBoard, newOpen, White, game.rows, game.cols, difficulty)
              TUI.printGameState(nextGame)
              gameLoop(nextGame, rand, history, limitSeconds, difficulty)
            }

          case _ =>
            println("Destino inválido ou salto impossível com esta peça. Tenta novamente.")
            handleMultiJump(game, rand, history, pieceCoord, limitSeconds, difficulty)
        }

      case _ =>
        handleMultiJump(game, rand, history, pieceCoord, limitSeconds, difficulty)
    }
  }
  // Converte "B3" -> (2, 1)  (row=dígito, col=letra)
  def parseCoord(input: String, rows: Int, cols: Int): Option[Coord2D] = {
    if (input.length < 2) return None
    val colChar = input.head.toUpper
    val rowStr  = input.tail
    if (!colChar.isLetter || !rowStr.forall(_.isDigit)) return None

    val col = colChar - 'A'
    val row = rowStr.toInt

    if (row >= 0 && row < rows && col >= 0 && col < cols)
      Some((row, col))
    else
      None
  }

  def coordToStr(coord: Coord2D): String = {
    val colChar = ('A' + coord._2).toChar
    s"$colChar${coord._1}"
  }
}