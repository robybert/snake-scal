package snake.logic

case class GameStack(start: GameState) { //in separate file or does not matter?

  private var frames: SStack[GameState] = SStack[GameState](start)
  private var reverseActive: Boolean = false

  def gameOver: Boolean = currentFrame.gameOverBool

  def step(): Unit = {
    if (reverseActive && frames.size > 1) frames = frames.pop
    else if (!currentFrame.gameOverBool && !reverseActive) frames = frames.push(currentFrame.moveSnake())
  }

  def setReverse(r: Boolean): Unit = {
    reverseActive = r
  }

  def cellTypeAt(p: Point): CellType = currentFrame.cellTypeAt(p)

  def changeDir(d: Direction): Unit = {
    if (d != currentFrame.currentDirection.opposite) {
      val replaceFrame = currentFrame
      frames = frames.pop
      frames = frames.push(replaceFrame.changeDir(d))
    }
  }

  private def currentFrame: GameState = frames.top
}
