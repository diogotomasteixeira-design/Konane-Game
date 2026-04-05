import Types.Stone

object Main {
  var player:Stone = Stone.Black
  val rand = MyRandom.create()
  val (board, lstOpenCoords) = Game.initBoard(6,6)
  
  Game.playRandomly(board, rand, player, lstOpenCoords, Game.randomMove)

}
