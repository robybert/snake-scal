package snake.logic

import engine.random.RandomGenerator

/** To implement Snake, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``snake`` package.
 */
class GameLogic(val random: RandomGenerator,
                val gridDims: Dimensions) {
  val gameStack = initGameStack()

  def gameOver: Boolean = gameStack.gameOver

  def step(): Unit = gameStack.step()


  def setReverse(r: Boolean): Unit = gameStack.setReverse(r)

  def changeDir(d: Direction): Unit = gameStack.changeDir(d)

  def initGameStack(): GameStack = {
    var initGameState = GameState(dims = gridDims,
      apple = null,
      snake = List(Point(2, 0), Point(1, 0), Point(0, 0)),
      growCount = 0,
      currentDirection = East(),
      newDirection = East(),
      gameOverBool = false,
      logic = this)
    initGameState = initGameState.initSnake()
    val initGameStack: GameStack = GameStack(initGameState)
    return initGameStack
  }

  def getCellType(p: Point): CellType = gameStack.cellTypeAt(p)
}

/** GameLogic companion object */
object GameLogic {

  val FramesPerSecond: Int = 5 // change this to increase/decrease speed of game

  val DrawSizeFactor = 2.0 // increase this to make the game bigger (for high-res screens)
  // or decrease to make game smaller

  // These are the dimensions used when playing the game.
  // When testing the game, other dimensions are passed to
  // the constructor of GameLogic.
  //
  // DO NOT USE the variable DefaultGridDims in your code!
  //
  // Doing so will cause tests which have different dimensions to FAIL!
  //
  // In your code only use gridDims.width and gridDims.height
  // do NOT use DefaultGridDims.width and DefaultGridDims.height
  val DefaultGridDims
  : Dimensions =
  Dimensions(width = 10, height = 10) // you can adjust these values to play on a different sized board
}


