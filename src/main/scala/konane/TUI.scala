package konane

import konane.Types.Stone.{Black, White}
import konane.Types.{Board, Coord2D, Stone}

import scala.annotation.tailrec
import scala.io.StdIn.readLine

case class TUI(gameState: Game) {
  def printGameState(): Unit = TUI.printGameState(gameState)
}


object TUI {
  def mainMenu(): (Int, Int, Long, Int) = {
    println("\n=== CONFIGURAÇÃO DO JOGO ===")

    print("Linhas (default 6): ")
    val rows = readLine().toIntOption.getOrElse(6)

    print("Colunas (default 6): ")
    val cols = readLine().toIntOption.getOrElse(6)

    print("Tempo limite por jogada em segundos (default 10): ")
    val time = readLine().toLongOption.getOrElse(10L)

    print("Dificuldade [1-Fácil, 2-Médio] (default 1): ")
    val diff = readLine().toIntOption.getOrElse(1)

    (rows, cols, time, diff)
  }

  def printGameState(gameState: Game): Unit = {
    val rows = gameState.rows
    val cols = gameState.cols
    val letra = 'A'

    println()
    println("|---------Konane Game--------|")
    println()

    @tailrec
    def loop(r: Int, c: Int): Unit = {
      (r, c) match {
        case (num1, _) if((num1 == rows + 1)) =>
        case (num1, num2) if(num2 == cols + 1) => println(); loop(num1 + 1, 0)
        case (0, 0) => print("   "); loop(0, 1)
        case (0, num) => print(s" ${(letra + num - 1).toChar}  "); loop(0, num + 1)
        case (num1, 0) => print(f"${num1 - 1}%2d |") ; loop(num1, 1)
        case (num1, num2) => {
          if(gameState.board.contains(num1 - 1, num2 - 1)) {
            if(gameState.board.get(num1 - 1, num2 - 1).contains(Black)) print(" B |")
            else print(" W |")
          }
          else if(gameState.lstOpenCoords.contains(num1 - 1, num2 - 1)) {
            print("   |")
          }
          else print("   |")
          loop(num1, num2 + 1)
        }
      }
    }
    loop(0,0)

    val vez = if(gameState.player == Black) "Pretas (B)" else "Brancas (W)"
    println()
    println(s"|------Vez de jogar: ${vez}------|")
    println()
  }
}