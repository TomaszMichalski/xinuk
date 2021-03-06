package pl.edu.agh.continuous.env.algorithm

import pl.edu.agh.continuous.env.config.ContinuousEnvConfig
import pl.edu.agh.continuous.env.model.ContinuousEnvCell
import pl.edu.agh.continuous.env.model.continuous.{CellOutline, Obstacle}
import pl.edu.agh.xinuk.algorithm.WorldCreator
import pl.edu.agh.xinuk.model.continuous.{Boundary, GridMultiCellId, Neighbourhood, Segment}
import pl.edu.agh.xinuk.model.grid.GridDirection.{Bottom, BottomLeft, BottomRight, Left, Right, Top, TopLeft, TopRight}
import pl.edu.agh.xinuk.model.{CellState, Signal, WorldBuilder}
import pl.edu.agh.xinuk.model.grid.{GridCellId, GridDirection, GridWorldBuilder}

import java.awt.geom.Area
import java.awt.Polygon
import scala.collection.mutable
import scala.collection.mutable.{Map => MutableMap}
import scala.swing.Rectangle
import scala.util.Random


object ContinuousEnvWorldCreator extends WorldCreator[ContinuousEnvConfig] {

  private val random = new Random(System.nanoTime())

  override def prepareWorld()(implicit config: ContinuousEnvConfig): WorldBuilder = {
    val worldBuilder = GridWorldBuilder().withGridConnections()

    var multiCellIdMap: MutableMap[GridCellId, Int] = MutableMap.empty
    var cellQueue: mutable.Queue[GridMultiCellId] = mutable.Queue.empty

    for {
      x <- 0 until config.worldWidth
      y <- 0 until config.worldHeight
    } {
      cellQueue += GridMultiCellId(x, y, 0)
      multiCellIdMap(GridCellId(x, y)) = 0
    }

    while (cellQueue.nonEmpty) {
      val gridMultiCellId = cellQueue.dequeue()
      val x = gridMultiCellId.x
      val y = gridMultiCellId.y
      /*val continuousEnvCell: ContinuousEnvCell = if (random.nextDouble() < config.signalSpawnChance) {
        ContinuousEnvCell(config.initialSignal)
      } else {
        ContinuousEnvCell(Signal.zero)
      }*/
      val continuousEnvCell: ContinuousEnvCell = ContinuousEnvCell(Signal.zero)
      continuousEnvCell.neighbourhood = worldBuilder.getExistingNeighbourhood(gridMultiCellId)

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

          if (isObstaclesGroupDividingCell(continuousEnvCell.cellOutline, obstaclesToMerge, x, y)) {
            println("Cell division detected")
            val newCells = divideCells(continuousEnvCell, obstaclesToMerge, x, y)
              .filter(cell => !hasEmptyNeighbourhood(cell))
              .filter(cell => !hasSameNeighbourhood(cell, continuousEnvCell))

            val currentId = multiCellIdMap(GridCellId(x, y))
            val currentGridMultiCellId = GridMultiCellId(x, y, currentId)
            val newCellNeighbourhoodMap: MutableMap[GridMultiCellId, Neighbourhood] = MutableMap.empty
            for (i <- newCells.indices) {
              val nextId = currentId + i + 1
              val newCellGridMultiCellId = GridMultiCellId(x, y, nextId)
              newCellNeighbourhoodMap(newCellGridMultiCellId) = newCells(i).neighbourhood
              cellQueue.enqueue(newCellGridMultiCellId)
            }
            worldBuilder.updateNeighbourhoodAfterDividingCell(currentGridMultiCellId, Map.from(newCellNeighbourhoodMap))
            multiCellIdMap(GridCellId(x, y)) = currentId + newCells.length
          }
        }
      }

      worldBuilder(gridMultiCellId) = CellState(continuousEnvCell)
    }

    worldBuilder
  }

  private def getOverlappingObstacles(continuousEnvCell: ContinuousEnvCell, obstacles: List[Obstacle], x: Int, y: Int)
                                     (implicit config: ContinuousEnvConfig): List[Obstacle] = {
    val cellOutline = continuousEnvCell.cellOutline
    val xScale = y
    val yScale = config.worldWidth - x - 1

    val cellOutlineArea = new Area(new Rectangle(
      xScale * config.cellSize + cellOutline.x.intValue, yScale * config.cellSize + cellOutline.y.intValue,
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

  private def isObstaclesGroupDividingCell(cellOutline: CellOutline, obstaclesGroup: Array[Obstacle], cellX: Int, cellY: Int)
                                          (implicit config: ContinuousEnvConfig): Boolean = {
    var obstacle = mergeToObstacle(obstaclesGroup)
    obstacle = addDummyPointsBetweenPointsLyingOnEdges(cellOutline, obstacle, cellX, cellY)
    val flags = pointsOnCellOutline(cellOutline, obstacle, cellX, cellY)
    val flips = countFlips(flags)

    flips > 2
  }

  private def mergeToObstacle(obstaclesGroup: Array[Obstacle]): Obstacle = {
    val mergedObstacleArea = new Area()
    for (obstacle <- obstaclesGroup) {
      val obstaclePoly = new Polygon(obstacle.xs, obstacle.ys, obstacle.points)
      mergedObstacleArea.add(new Area(obstaclePoly))
    }

    toObstacle(mergedObstacleArea)
  }

  private def addDummyPointsBetweenPointsLyingOnEdges(cellOutline: CellOutline, obstacle: Obstacle, cellX: Int, cellY: Int)
                                                     (implicit config: ContinuousEnvConfig): Obstacle = {
    var newXs: Array[Int] = Array()
    var newYs: Array[Int] = Array()

    for (i <- 0 until obstacle.points) {
      val currentX = obstacle.xs(i)
      val currentY = obstacle.ys(i)
      val nextX = obstacle.xs((i + 1) % obstacle.points)
      val nextY = obstacle.ys((i + 1) % obstacle.points)
      if (!isLyingInsideCellOutline(cellOutline, currentX, currentY, cellX, cellY)
          && !isLyingInsideCellOutline(cellOutline, nextX, nextY, cellX, cellY)) {
        val dummyX = (currentX + nextX) / 2
        val dummyY = (currentY + nextY) / 2

        newXs = newXs :+ currentX :+ dummyX
        newYs = newYs :+ currentY :+ dummyY
      } else {
        newXs = newXs :+ currentX
        newYs = newYs :+ currentY
      }
    }

    new Obstacle(newXs, newYs, newXs.length)
  }

  private def isLyingInsideCellOutline(cellOutline: CellOutline, x: Int, y: Int, cellX: Int, cellY: Int)
                                      (implicit config: ContinuousEnvConfig): Boolean = {
    val xScale = cellY
    val yScale = config.worldWidth - cellX - 1

    val localX = x - xScale * config.cellSize
    val localY = y - yScale * config.cellSize
    localX > cellOutline.x && localX < cellOutline.x + cellOutline.width && localY > cellOutline.y && localY < cellOutline.y + cellOutline.height
  }

  private def pointsOnCellOutline(cellOutline: CellOutline, obstacle: Obstacle, cellX: Int, cellY: Int)
                                 (implicit config: ContinuousEnvConfig): Array[Boolean] = {
    var flags: Array[Boolean] = Array()
    for (i <- 0 until obstacle.points) {
      val currentX = obstacle.xs(i)
      val currentY = obstacle.ys(i)
      flags = flags :+ !isLyingInsideCellOutline(cellOutline, currentX, currentY, cellX, cellY)
    }

    flags
  }

  private def countFlips(flags: Array[Boolean]): Int = {
    var flips = 0
    for (i <- flags.indices) {
      if (flags(i) != flags((i + 1) % flags.length)) {
        flips = flips + 1
      }
    }

    flips
  }

  private def divideCells(cell: ContinuousEnvCell, obstaclesGroup: Array[Obstacle], cellX: Int, cellY: Int)
                         (implicit config: ContinuousEnvConfig): Array[ContinuousEnvCell] = {
    var obstacle = mergeToObstacle(obstaclesGroup)
    obstacle = addDummyPointsBetweenPointsLyingOnEdges(cell.cellOutline, obstacle, cellX, cellY)
    val flags = pointsOnCellOutline(cell.cellOutline, obstacle, cellX, cellY)

    val newCellBoundaries: Array[Array[(Int, Int)]] = extractNewCellBoundaries(cell.cellOutline, obstacle, flags, cellX, cellY)

    val newCells = newCellBoundaries
      .map(cellBoundary => createNewCell(cell, cellBoundary))

    newCells
  }

  private def extractNewCellBoundaries(cellOutline: CellOutline, obstacle: Obstacle, flags: Array[Boolean], cellX: Int, cellY: Int)
                                      (implicit config: ContinuousEnvConfig): Array[Array[(Int, Int)]] = {
    val localObstaclePoints: Array[(Int, Int)] = toLocalObstaclePoints(obstacle, cellX, cellY)
    var newCellBoundaries: Array[Array[(Int, Int)]] = Array()
    val newCellsNum = countFlips(flags) / 2

    var start = 0
    for (i <- 0 until newCellsNum) {
      val nextTrueBeforeFalse = findNextTrueBeforeFalse(flags, start)
      val nextTrue = findNextTrue(flags, nextTrueBeforeFalse + 1)

      var cellSplit: Array[(Int, Int)] = Array()
      for (j <- nextTrueBeforeFalse to nextTrue) {
        cellSplit = cellSplit :+ localObstaclePoints(j % localObstaclePoints.length)
      }

      cellSplit = addCellOutlineVerticesToSplit(cellOutline, cellSplit)

      newCellBoundaries = newCellBoundaries :+ cellSplit
      start = nextTrue + 1
    }

    newCellBoundaries
  }

  private def toLocalObstaclePoints(obstacle: Obstacle, cellX: Int, cellY: Int)
                                   (implicit config: ContinuousEnvConfig): Array[(Int, Int)] = {
    var localObstaclePoints: Array[(Int, Int)] = Array()
    for (i <- 0 until obstacle.points) {
      val xScale = cellY
      val yScale = config.worldWidth - cellX - 1

      val localX = obstacle.xs(i) - xScale * config.cellSize
      val localY = obstacle.ys(i) - yScale * config.cellSize

      localObstaclePoints = localObstaclePoints :+ (localX, localY)
    }

    localObstaclePoints
  }

  private def findNextTrueBeforeFalse(flags: Array[Boolean], start: Int): Int = {
    var result = -1
    var found = false
    for (i <- 0 to flags.length if !found) {
      if (flags((start + i) % flags.length) && !flags((start + i + 1) % flags.length)) {
        result = start + i
        found = true
      }
    }

    result
  }

  private def findNextTrue(flags: Array[Boolean], start: Int): Int = {
    var result = -1
    var found = false
    for (i <- 0 to flags.length if !found) {
      if (flags((start + i) % flags.length)) {
        result =  start + i
        found = true
      }
    }

    result
  }

  private def addCellOutlineVerticesToSplit(cellOutline: CellOutline, cellSplit: Array[(Int, Int)]): Array[(Int, Int)] = {
    val firstBoundaryNum = getBoundaryNumForPoint(cellOutline, cellSplit(0))
    var lastBoundaryNum = getBoundaryNumForPoint(cellOutline, cellSplit(cellSplit.length - 1))

    val cellOutlineVertices: Array[(Int, Int)] = Array((cellOutline.x, cellOutline.y + cellOutline.height),
                                                       (cellOutline.x + cellOutline.width, cellOutline.y + cellOutline.height),
                                                       (cellOutline.x + cellOutline.width, cellOutline.y),
                                                       (cellOutline.x, cellOutline.y))

    var verticesToAdd: Array[(Int, Int)] = Array()
    if (lastBoundaryNum < firstBoundaryNum) {
      lastBoundaryNum = lastBoundaryNum + 4
    }
    for (i <- firstBoundaryNum until lastBoundaryNum) {
      if (cellOutlineVertices(i % 4) != cellSplit(0)) {
        verticesToAdd = verticesToAdd :+ cellOutlineVertices(i % 4)
      }
    }

    cellSplit.reverse :++ verticesToAdd
  }

  private def getBoundaryNumForPoint(cellOutline: CellOutline, point: (Int, Int)): Int = {
    val x = point._1
    val y = point._2

    var boundaryNum = -1
    if (x == cellOutline.x && y > cellOutline.y && y <= cellOutline.y + cellOutline.height) {
      boundaryNum = 0
    } else if (x > cellOutline.x && x <= cellOutline.x + cellOutline.width && y == cellOutline.y + cellOutline.height) {
      boundaryNum = 1
    } else if (x == cellOutline.x + cellOutline.width && y >= cellOutline.y && y < cellOutline.y + cellOutline.height) {
      boundaryNum = 2
    } else if (x <= cellOutline.x + cellOutline.width && x > cellOutline.x && y == cellOutline.y) {
      boundaryNum = 3
    }

    boundaryNum
  }

  private def createNewCell(existingCell: ContinuousEnvCell, newCellBoundary: Array[(Int, Int)])
                           (implicit config:ContinuousEnvConfig): ContinuousEnvCell = {
    val existingNeighbourhood = existingCell.neighbourhood
    var newCellNeighbourhood = Neighbourhood.empty()
    val cardinalNeighbourhood: MutableMap[GridDirection, Boundary] = MutableMap.from(newCellNeighbourhood.cardinalNeighbourhood)
    val diagonalNeighbourhood: MutableMap[GridDirection, GridMultiCellId] = MutableMap.from(newCellNeighbourhood.diagonalNeighbourhood)

    for (i <- newCellBoundary.indices) {
      val start = newCellBoundary(i)
      val end = newCellBoundary((i + 1) % newCellBoundary.length)
      if (isVertical(start, end)) { // can be left or right
        var segment = Segment(start._2, end._2)
        if (isLeft(existingCell, start)) {
          cardinalNeighbourhood(Left) = getNewCardinalBoundary(existingNeighbourhood, Left, segment)
        } else if (isRight(existingCell, start)) {
          segment = Segment(segment.b, segment.a) // need to mirror in case it's right boundary
          cardinalNeighbourhood(Right) = getNewCardinalBoundary(existingNeighbourhood, Right, segment)
        }
      } else if (isHorizontal(start, end)) { // can be top or bottom
        var segment = Segment(start._1, end._1)
        if (isTop(existingCell, start)) {
          cardinalNeighbourhood(Top) = getNewCardinalBoundary(existingNeighbourhood, Top, segment)
        } else if (isBottom(existingCell, start)) {
          segment = Segment(segment.b, segment.a) // need to mirror in case it's bottom boundary
          cardinalNeighbourhood(Bottom) = getNewCardinalBoundary(existingNeighbourhood, Bottom, segment)
        }
      }
    }

    for (i <- newCellBoundary.indices) {
      val point = newCellBoundary(i)
      if (isTopLeft(existingCell, point)) {
        diagonalNeighbourhood(TopLeft) = existingNeighbourhood.diagonalNeighbourhood(TopLeft)
      } else if (isTopRight(existingCell, point)) {
        diagonalNeighbourhood(TopRight) = existingNeighbourhood.diagonalNeighbourhood(TopRight)
      } else if (isBottomRight(existingCell, point)) {
        diagonalNeighbourhood(BottomRight) = existingNeighbourhood.diagonalNeighbourhood(BottomRight)
      } else if (isBottomLeft(existingCell, point)) {
        diagonalNeighbourhood(BottomLeft) = existingNeighbourhood.diagonalNeighbourhood(BottomLeft)
      }
    }

    newCellNeighbourhood = Neighbourhood(Map.from(cardinalNeighbourhood), Map.from(diagonalNeighbourhood))
    val newCell = ContinuousEnvCell(existingCell.initialSignal)
    newCell.cellOutline = getNewCellOutline(newCellBoundary)
    newCell.neighbourhood = newCellNeighbourhood

    newCell
  }

  private def isVertical(start: (Int, Int), end: (Int, Int)): Boolean = {
    start._1 == end._1
  }

  private def isHorizontal(start: (Int, Int), end: (Int, Int)): Boolean = {
    start._2 == end._2
  }

  private def isLeft(continuousEnvCell: ContinuousEnvCell, point: (Int, Int)): Boolean = {
    continuousEnvCell.cellOutline.x == point._1
  }

  private def isRight(continuousEnvCell: ContinuousEnvCell, point: (Int, Int)): Boolean = {
    continuousEnvCell.cellOutline.x + continuousEnvCell.cellOutline.width == point._1
  }

  private def isTop(continuousEnvCell: ContinuousEnvCell, point: (Int, Int)): Boolean = {
    continuousEnvCell.cellOutline.y + continuousEnvCell.cellOutline.height == point._2
  }

  private def isBottom(continuousEnvCell: ContinuousEnvCell, point: (Int, Int)): Boolean = {
    continuousEnvCell.cellOutline.y == point._2
  }

  private def isTopLeft(continuousEnvCell: ContinuousEnvCell, point: (Int, Int)): Boolean = {
    val cellOutline = continuousEnvCell.cellOutline
    cellOutline.x == point._1 && cellOutline.y + cellOutline.height == point._2
  }

  private def isTopRight(continuousEnvCell: ContinuousEnvCell, point: (Int, Int)): Boolean = {
    val cellOutline = continuousEnvCell.cellOutline
    cellOutline.x + cellOutline.width == point._1 && cellOutline.y + cellOutline.height == point._2
  }

  private def isBottomRight(continuousEnvCell: ContinuousEnvCell, point: (Int, Int)): Boolean = {
    val cellOutline = continuousEnvCell.cellOutline
    cellOutline.x + cellOutline.width == point._1 && cellOutline.y == point._2
  }

  private def isBottomLeft(continuousEnvCell: ContinuousEnvCell, point: (Int, Int)): Boolean = {
    val cellOutline = continuousEnvCell.cellOutline
    cellOutline.x == point._1 && cellOutline.y == point._2
  }

  private def getNewCellOutline(boundary: Array[(Int, Int)]): CellOutline = {
    val minX = boundary.map(point => point._1).min
    val maxX = boundary.map(point => point._1).max
    val minY = boundary.map(point => point._2).min
    val maxY = boundary.map(point => point._2).max

    CellOutline(minX, minY, maxX - minX, maxY - minY)
  }

  private def getNewCardinalBoundary(existingNeighbourhood: Neighbourhood, cardinalDirection: GridDirection, segment: Segment): Boundary = {
    val boundaryMap = existingNeighbourhood
      .cardinalNeighbourhood(cardinalDirection)
      .boundaries
      .map { case (boundarySegment, gridMultiCellId) => (getCommonSegment(segment, boundarySegment), gridMultiCellId) }
      .filter { case (commonSegment, _) => commonSegment != null }

    Boundary(boundaryMap)
  }

  private def getCommonSegment(segment: Segment, boundarySegment: Segment): Segment = {
    val a = scala.math.max(segment.a, boundarySegment.a)
    val b = scala.math.min(segment.b, boundarySegment.b)

    if (a > b) {
      null
    } else {
      Segment(a, b)
    }
  }

  private def hasEmptyNeighbourhood(continuousEnvCell: ContinuousEnvCell): Boolean = {
    val emptyDiagonalNeighbourhood = continuousEnvCell
      .neighbourhood
      .diagonalNeighbourhood
      .forall { case (_, gridMultiCellId) => gridMultiCellId == null }
    val emptyCardinalNeighbourhood = continuousEnvCell
      .neighbourhood
      .cardinalNeighbourhood
      .forall { case (_, boundary) => boundary.boundaries == Map.empty }

    emptyDiagonalNeighbourhood && emptyCardinalNeighbourhood
  }

  private def hasSameNeighbourhood(cell: ContinuousEnvCell, other: ContinuousEnvCell): Boolean = {
    val hasSameDiagonalNeighbourhood = cell
      .neighbourhood
      .diagonalNeighbourhood
      .forall { case (direction, gridMultiCellId) => gridMultiCellId == other.neighbourhood.diagonalNeighbourhood(direction) }
    val hasSameCardinalNeighbourhood = cell
      .neighbourhood
      .cardinalNeighbourhood
      .forall { case (direction, boundary) => boundary.boundaries == other.neighbourhood.cardinalNeighbourhood(direction).boundaries }

    hasSameDiagonalNeighbourhood && hasSameCardinalNeighbourhood
  }
}
