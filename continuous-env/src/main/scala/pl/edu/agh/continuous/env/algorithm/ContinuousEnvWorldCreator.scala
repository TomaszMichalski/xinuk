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

      val contents: CellContents = if (x == 9 && y == 9) {
        ContinuousEnvCell(config.initialSignal)
      } else if (x == 1 && y == 18) {
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
      y <= 5 || y >= 18
    } else if (x == 1) {
      y <= 2 || y == 19
    } else if (x == 2) {
      y <= 1 || (y >= 9 && y <= 13)
    } else if (x == 3) {
      y == 0 || (y >= 7 && y <= 15)
    } else if (x == 4) {
      y == 0 || (y >= 5 && y <= 16)
    } else if (x == 5) {
      y == 0 || (y >= 4 && y <= 7) || (y >= 15 && y <= 18)
    } else if (x == 6) {
      y == 4 || y == 5 || y >= 17
    } else if (x == 7) {
      (y >= 3 && y <= 5) || (y >= 8 && y <= 13) || y >= 18
    } else if (x == 8) {
      y == 3 || y == 4 || (y >= 7 && y <= 14) || y >= 18
    } else if (x == 9) {
      y == 3 || y == 4 || y == 7 || y == 8 || y == 14 || y == 15 || y >= 18
    } else if (x == 10) {
      y == 3 || y == 4 || y == 7 || y == 8 || y == 9 || y == 14 || y == 15 || y >= 18
    } else if (x == 11) {
      y == 3 || y == 4 || y == 7 || y == 8 || y == 9 || y == 14 || y == 15 || y >= 18
    } else if (x == 12) {
      y == 3 || y == 4 || y == 14 || y == 15 || y >= 18
    } else if (x == 13) {
      y == 0 || y == 4 || y == 5 || (y >= 13 && y <= 15) || y >= 18
    } else if (x == 14) {
      y == 0 || (y >= 5 && y <= 14) || y >= 18
    } else if (x == 15) {
      y == 0 || y == 1 || (y >= 6 && y <= 13) || y >= 17
    } else if (x == 16) {
      y <= 2 || y >= 17
    } else if (x == 17) {
      y <= 4 || y >= 15
    } else if (x == 18) {
      true
    } else if (x == 19) {
      true
    } else {
      false
    }

    result
  }
}
