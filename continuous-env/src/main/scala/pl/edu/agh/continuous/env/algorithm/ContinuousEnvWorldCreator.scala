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

      val contents: CellContents = if (x == 19 && y == 19) {
        ContinuousEnvCell(config.initialSignal)
      } else if (x == 3 && y == 36) {
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
      y <= 13 || y >= 36
    } else if (x == 1) {
      y <= 10 || y >= 38
    } else if (x == 2) {
      y <= 8 || y == 39
    } else if (x == 3) {
      y <= 6
    } else if (x == 4) {
      y <= 4
    } else if (x == 5) {
      y <= 3 || (y >= 19 && y <= 27)
    } else if (x == 6) {
      y <= 3 || (y >= 14 && y <= 30)
    } else if (x == 7) {
      y <= 2 || (y >= 13 && y <= 33)
    } else if (x == 8) {
      y <= 2 || (y >= 11 && y <= 34)
    } else if (x == 9) {
      y <= 1 || (y >= 10 && y <= 19) || (y >= 25 && y <= 35)
    } else if (x == 10) {
      y <= 1 || (y >= 9 && y <= 15) || (y >= 28 && y <= 36)
    } else if (x == 11) {
      y <= 1 || (y >= 9 && y <= 14) || (y >= 31 && y <= 37)
    } else if (x == 12) {
      y == 0 || (y >= 8 && y <= 12) || (y >= 33 && y <= 38)
    } else if (x == 13) {
      y == 0 || (y >= 7 && y <= 11) || (y >= 20 && y <= 24) || (y >= 34 && y <= 38)
    } else if (x == 14) {
      y == 0 || (y >= 7 && y <= 10) || (y >= 16 && y <= 26) || y >= 35
    } else if (x == 15) {
      (y >= 7 && y <= 9) || (y >= 15 && y <= 27) || y >= 35
    } else if (x == 16) {
      (y >= 6 && y <= 9) || (y >= 15 && y <= 28) || y >= 35
    } else if (x == 17) {
      (y >= 6 && y <= 8) || (y >= 14 && y <= 19) || (y >= 25 && y <= 29) || y >= 35
    } else if (x == 18) {
      (y >= 6 && y <= 8) || (y >= 14 && y <= 18) || (y >= 26 && y <= 30) || y >= 36
    } else if (x == 19) {
      (y >= 5 && y <= 8) || (y >= 14 && y <= 18) || (y >= 27 && y <= 30) || y >= 36
    } else if (x == 20) {
      (y >= 5 && y <= 8) || (y >= 14 && y <= 18) || (y >= 27 && y <= 30) || y >= 36
    } else if (x == 21) {
      (y >= 5 && y <= 8) || (y >= 14 && y <= 19) || (y >= 28 && y <= 30) || y >= 36
    } else if (x == 22) {
      (y >= 6 && y <= 8) || (y >= 14 && y <= 19) || (y >= 28 && y <= 30) || y >= 36
    } else if (x == 23) {
      y == 0 || (y >= 6 && y <= 8) || (y >= 15 && y <= 19) || (y >= 28 && y <= 30) || y >= 36
    } else if (x == 24) {
      y == 0 || (y >= 6 && y <= 8) || (y >= 15 && y <= 19) || (y >= 28 && y <= 30) || y >= 36
    } else if (x == 25) {
      y == 0 || (y >= 7 && y <= 9) || (y >= 27 && y <= 30) || y >= 36
    } else if (x == 26) {
      y == 0 || (y >= 7 && y <= 10) || (y >= 27 && y <= 30) || y >= 36
    } else if (x == 27) {
      y <= 1 || (y >= 7 && y <= 11) || (y >= 26 && y <= 30) || y >= 36
    } else if (x == 28) {
      y <= 1 || (y >= 8 && y <= 13) || (y >= 24 && y <= 29) || y >= 36
    } else if (x == 29) {
      y <= 2 || (y >= 9 && y <= 29) || y >= 35
    } else if (x == 30) {
      y <= 3 || (y >= 10 && y <= 27) || y >= 35
    } else if (x == 31) {
      y <= 4 || (y >= 12 && y <= 25) || y >= 35
    } else if (x == 32) {
      y <= 5 || (y >= 15 && y <= 24) || y >= 34
    } else if (x == 33) {
      y <= 6 || y >= 33
    } else if (x == 34) {
      y <= 7 || y >= 32
    } else if (x == 35) {
      y <= 8 || y >= 30
    } else if (x == 36) {
      y <= 11 || y >= 27
    } else if (x == 37) {
      y <= 16 || y >= 23
    } else if (x == 38) {
      true
    } else if (x == 39) {
      true
    } else {
      false
    }

    result
  }
}
