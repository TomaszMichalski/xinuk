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

      val contents: CellContents = if (x == 2 && y == 2) {
        ContinuousEnvCell(config.initialSignal)
      } else if (x == 0 && y == 4) {
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
    var result = false
    if (x == 1) {
      result = y > 0
    } else if (x == 2) {
      result = y == 1 || y == 4
    } else if (x == 3) {
      result = y == 1 || y == 2 || y == 4
    } else if (x == 4) {
      result = y == 4
    }

    result
  }
}
