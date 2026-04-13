import Types.{Board, Coord2D, Stone}
import Stone.{Black, White}
import scala.io.StdIn.readLine
import scala.annotation.tailrec

object Main {

  def main(args: Array[String]): Unit = {
    println("=== Bem-vindo ao Kōnane ===")
    println("Tu jogas com as Pretas (B). O computador joga com as Brancas (W).")
    println()

    val rows = 6
    val cols = 6
    val (board, lstOpenCoords) = Game.initBoard(rows, cols)
    val initialGame = Game(board, lstOpenCoords, Black, rows, cols)
    val rand = MyRandom.create()

    TUI.printGameState(initialGame)
    gameLoop(initialGame, rand)
  }

  @tailrec
  @tailrec
  def gameLoop(game: Game, rand: MyRandom): Unit = {

    if (!Game.hasValidMoves(game.board, game.player, game.lstOpenCoords)) {
      val vencedor = if (game.player == Black) "O computador (Brancas) ganhou!" else "Tu (Pretas) ganhaste!"
      println(s"\n=== Fim de jogo! $vencedor ===")
    } else {
      game.player match {
        case Black =>
          println("A tua vez! Escolhe uma opção:")
          println("  1 - Introduzir coordenada de destino manualmente")
          println("  2 - Jogada aleatória")

          val opcao = readLine("Opção: ").trim

          opcao match {
            case "1" =>
              handleHumanMove(game, rand)

            case "2" =>
              println("A jogar aleatoriamente...")
              val (optBoard, nextRand, nextOpen, optCoord) =
                Game.playRandomly(game.board, rand, game.player, game.lstOpenCoords, Game.randomMove)

              optBoard match {
                case Some(newBoard) =>
                  println(s"Jogaste para: ${coordToStr(optCoord.get)}")
                  val nextGame = Game(newBoard, nextOpen, White, game.rows, game.cols)
                  TUI.printGameState(nextGame)
                  gameLoop(nextGame, nextRand)
                case None =>
                  println("\n=== Não existem mais movimentos! O computador (Brancas) ganhou! ===")
              }

            case _ =>
              println("Opção inválida, tenta novamente.")
              gameLoop(game, rand)
          }

        case White =>
          println("Vez do computador (Brancas)...")
          val (optBoard, nextRand, nextOpen, optCoord) =
            Game.playRandomly(game.board, rand, game.player, game.lstOpenCoords, Game.randomMove)

          optBoard match {
            case Some(newBoard) =>
              println(s"O computador jogou para: ${coordToStr(optCoord.get)}")
              val nextGame = Game(newBoard, nextOpen, Black, game.rows, game.cols)
              TUI.printGameState(nextGame)
              gameLoop(nextGame, nextRand)
            case None =>
              println("\n=== Não existem mais movimentos! Tu (Pretas) ganhaste! ===")
          }
      }
    }
  }

  def handleHumanMove(game: Game, rand: MyRandom): Unit = {
    println("Introduz a coordenada de destino (ex: B3 ou b3):")
    val input = readLine("Destino: ").trim

    parseCoord(input, game.rows, game.cols) match {
      case None =>
        println("Coordenada inválida. Tenta novamente.")
        handleHumanMove(game, rand)

      case Some(coordTo) =>
        Game.findPieceForMove(game.board, game.player, coordTo, game.lstOpenCoords) match {
          case None =>
            println(s"Não existe nenhuma peça tua que possa mover para ${coordToStr(coordTo)}. Tenta outra coordenada.")
            handleHumanMove(game, rand)

          case Some(coordFrom) =>
            val (optBoard, newOpen) = Game.play(game.board, game.player, coordFrom, coordTo, game.lstOpenCoords)
            optBoard match {
              case Some(newBoard) =>
                println(s"Moveste de ${coordToStr(coordFrom)} para ${coordToStr(coordTo)}.")
                val nextGame = Game(newBoard, newOpen, White, game.rows, game.cols)
                TUI.printGameState(nextGame)
                gameLoop(nextGame, rand)
              case None =>
                println("Jogada inválida. Tenta novamente.")
                handleHumanMove(game, rand)
            }
        }
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