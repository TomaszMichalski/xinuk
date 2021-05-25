package pl.edu.agh.continuous.env.algorithm

import pl.edu.agh.continuous.env.config.ContinuousEnvConfig
import pl.edu.agh.continuous.env.model.{ContinuousEnvBeing, ContinuousEnvCell}
import pl.edu.agh.xinuk.algorithm.WorldCreator
import pl.edu.agh.xinuk.model.{CellContents, CellState, Obstacle, Signal, WorldBuilder}
import pl.edu.agh.xinuk.model.grid.{GridCellId, GridWorldBuilder}

import scala.util.Random

object ContinuousEnvWorldCreator extends WorldCreator[ContinuousEnvConfig] {

  private val random = new Random(System.nanoTime())

  override def prepareWorld()(implicit config: ContinuousEnvConfig): WorldBuilder = {
    val worldBuilder = GridWorldBuilder().withGridConnections()

    for {
      x <- 0 until config.worldWidth
      y <- 0 until config.worldHeight
    } {

      val contents: CellContents = if (x == 5 && y == 5) {
        ContinuousEnvCell(config.initialSignal)
      } else if (x == 1 && y == 9) {
        ContinuousEnvBeing()
      } else if (isObstacle(x, y)) {
        Obstacle
      } else {
        ContinuousEnvCell(Signal.zero)
      }

      worldBuilder(GridCellId(x, y)) = CellState(contents)
    }

    worldBuilder
  }

  private def isObstacle(x: Int, y: Int): Boolean = {
    val result = if (x == 0) {
      y == 0 || y == 1 || y == 9
    } else if (x == 1) {
      y == 0 || (y >= 3 && y <= 6)
    } else if (x == 2) {
      y == 2 || y == 3 || (y >= 6 && y <= 8)
    } else if (x == 3) {
      (y >= 1 && y <= 2) || y >= 7
    } else if (x == 4) {
      y == 1 || (y >= 4 && y <= 6) || y == 9
    } else if (x == 5) {
      (y >= 1 && y <= 2) || y == 4 || (y >= 6 && y <= 7) || y == 9
    } else if (x == 6) {
      y == 2 || y == 6 || y == 7 || y == 9
    } else if (x == 7) {
      y == 0 || (y >= 2 && y <= 6) || y == 9
    } else if (x == 8) {
      y == 0 || y >= 8
    } else if (x == 9) {
      y <= 2 || y >= 7
    } else {
      false
    }

    result
  }
}
