package pl.edu.agh.continuous.env.algorithm

import pl.edu.agh.continuous.env.config.ContinuousEnvConfig
import pl.edu.agh.continuous.env.model.ContinuousEnvCell
import pl.edu.agh.continuous.env.model.continuous.{CellOutline, Obstacle}
import pl.edu.agh.xinuk.algorithm.WorldCreator
import pl.edu.agh.xinuk.model.{CellState, Obstacle, Signal, WorldBuilder}
import pl.edu.agh.xinuk.model.grid.{GridCellId, GridWorldBuilder}

import java.awt.geom.Area
import java.awt.Polygon
import scala.swing.Rectangle
import scala.util.Random

object ContinuousEnvWorldCreator extends WorldCreator[ContinuousEnvConfig] {

  private val random = new Random(System.nanoTime())

  override def prepareWorld()(implicit config: ContinuousEnvConfig): WorldBuilder = {
    val worldBuilder = GridWorldBuilder().withGridConnections()

    for {
      x <- 0 until config.worldWidth
      y <- 0 until config.worldHeight
    } {
      val continuousEnvCell: ContinuousEnvCell = if (random.nextDouble() < config.signalSpawnChance) {
        ContinuousEnvCell(config.initialSignal)
      } else {
        ContinuousEnvCell(Signal.zero)
      }

      val obstacles = config.obstacles
      val overlappingObstacles = getOverlappingObstacles(continuousEnvCell, obstacles, x, y)

      if (overlappingObstacles.nonEmpty) {
        println("Found overlapping obstacles")
        var obstaclesGroups: Array[Array[Obstacle]] = Array()

        for (obstacle <- overlappingObstacles) {
          var obstaclesToMerge = Array(obstacle)
          var newObstaclesGroups: Array[Array[Obstacle]] = Array()
          for (existingObstacleGroup <- obstaclesGroups) {
            if (overlapsWithAny(obstacle, existingObstacleGroup)) {
              obstaclesToMerge = obstaclesToMerge ++ existingObstacleGroup
            } else {
              newObstaclesGroups = newObstaclesGroups :+ existingObstacleGroup
            }
          }
          newObstaclesGroups = newObstaclesGroups :+ obstaclesToMerge
          obstaclesGroups = newObstaclesGroups

          if (isObstaclesGroupDividingCell(continuousEnvCell.cellOutline, obstaclesToMerge)) {
            println("Cell should be divided")
          }
        }
      }

      worldBuilder(GridCellId(x, y)) = CellState(continuousEnvCell)
    }

    worldBuilder
  }

  private def getOverlappingObstacles(continuousEnvCell: ContinuousEnvCell, obstacles: List[Obstacle], x: Int, y: Int)
                                     (implicit config: ContinuousEnvConfig): List[Obstacle] = {
    val cellOutline = continuousEnvCell.cellOutline
    val xScale = y
    val yScale = config.worldWidth - x - 1

    val cellOutlineArea = new Area(new Rectangle(
      xScale * config.cellSize + cellOutline.x.intValue, yScale * config.cellSize.intValue,
      cellOutline.width.intValue, cellOutline.height.intValue))
    val obstaclesAreas = obstacles
      .map(obstacle => new Area(new Polygon(obstacle.xs, obstacle.ys, obstacle.points)))
    obstaclesAreas
      .foreach(obstacleArea => obstacleArea.intersect(cellOutlineArea))
    val overlappingObstaclesAreas = obstaclesAreas
      .filter(obstacleArea => !obstacleArea.isEmpty)
      .map(overlappingObstacleArea => toObstacle(overlappingObstacleArea))

    overlappingObstaclesAreas
  }

  private def toObstacle(overlappingObstacleArea: Area): Obstacle = {
    val pathIterator = overlappingObstacleArea.getPathIterator(null)
    val coords = Array(0d, 0d, 0d, 0d, 0d, 0d)
    var xs: Array[Int] = Array()
    var ys: Array[Int] = Array()
    while (!pathIterator.isDone) {
      pathIterator.currentSegment(coords)
      xs = xs :+ coords(0).intValue
      ys = ys :+ coords(1).intValue
      pathIterator.next()
    }

    xs = xs.dropRight(1)
    ys = ys.dropRight(1)

    new Obstacle(xs, ys, xs.length)
  }

  private def overlapsWithAny(obstacle: Obstacle, obstaclesGroup: Array[Obstacle]): Boolean = {
    obstaclesGroup.exists(existingObstacle => overlaps(obstacle, existingObstacle))
  }

  private def overlaps(obstacle: Obstacle, existingObstacle: Obstacle): Boolean = {
    anyPointLiesIn(obstacle, existingObstacle) ||
      anyPointLiesIn(existingObstacle, obstacle) ||
      anyPointLiesOn(obstacle, existingObstacle) ||
      anyPointLiesOn(existingObstacle, obstacle)
  }

  private def anyPointLiesIn(first: Obstacle, second: Obstacle): Boolean = {
    var result = false
    val poly = new Polygon(first.xs, first.ys, first.points)
    for (i <- 0 until second.points) {
      if (poly.contains(second.xs(i), second.ys(i))) {
        result = true
      }
    }

    result
  }

  private def anyPointLiesOn(first: Obstacle, second: Obstacle): Boolean = {
    var result = false
    for (i <- 0 until first.points) {
      val cX = first.xs(i)
      val cY = first.ys(i)
      for (j <- 0 until second.points) {
        val aX = second.xs(j)
        val aY = second.ys(j)
        val bX = second.xs((j + 1) % second.points)
        val bY = second.ys((j + 1) % second.points)

        if (squaredDistance(aX, aY, cX, cY) + squaredDistance(cX, cY, bX, bY) == squaredDistance(aX, aY, bX, bY)) {
          result = true
        }
      }
    }

    result
  }

  private def squaredDistance(x1: Int, y1: Int, x2: Int, y2: Int): Int = {
    (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)
  }

  private def isObstaclesGroupDividingCell(cellOutline: CellOutline, obstaclesGroup: Array[Obstacle]): Boolean = {
    false // TODO
  }
}
