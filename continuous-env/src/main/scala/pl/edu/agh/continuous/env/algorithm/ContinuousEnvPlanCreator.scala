package pl.edu.agh.continuous.env.algorithm

import pl.edu.agh.continuous.env.algorithm.ContinuousEnvUpdateTag.{Arrive, Leave, Stay}
import pl.edu.agh.continuous.env.config.ContinuousEnvConfig
import pl.edu.agh.continuous.env.model.ContinuousEnvCell
import pl.edu.agh.continuous.env.model.continuous.MovementDirection.MovementDirection
import pl.edu.agh.continuous.env.model.continuous.{Being, BeingMetadata, MovementDirection, Obstacle, ObstacleSegment, SignalVector}
import pl.edu.agh.xinuk.algorithm.{Plan, PlanCreator, Plans}
import pl.edu.agh.xinuk.model.continuous.{Boundary, GridMultiCellId, NeighbourhoodState, Segment}
import pl.edu.agh.xinuk.model.grid.GridDirection
import pl.edu.agh.xinuk.model.grid.GridDirection.{Bottom, BottomLeft, BottomRight, Left, Right, Top, TopLeft, TopRight}
import pl.edu.agh.xinuk.model.{CellId, CellState, Direction, Signal, SignalMap}

import scala.util.Random

final case class ContinuousEnvPlanCreator() extends PlanCreator[ContinuousEnvConfig] {

  private val random = new Random(System.nanoTime())

  override def createPlans(iteration: Long, cellId: CellId, cellState: CellState, neighbourhoodState: NeighbourhoodState)
                          (implicit config: ContinuousEnvConfig): (Plans, ContinuousEnvMetrics) = {
    cellState.contents match {
      case continuousEnvCell: ContinuousEnvCell => processMovement(continuousEnvCell, cellState, neighbourhoodState)
      case _ => (Plans.empty, ContinuousEnvMetrics.empty)
    }
  }

  private def processMovement(continuousEnvCell: ContinuousEnvCell, cellState: CellState, neighbourhoodState: NeighbourhoodState)
                             (implicit config: ContinuousEnvConfig): (Plans, ContinuousEnvMetrics) = {
    val plans: Plans = if (continuousEnvCell.being == null) {
        Plans.empty
      } else {
        val signalVector = signalMapToSignalVec(cellState.signalMap)

        if (signalVector != SignalVector.zero) {
          if (continuousEnvCell.beingMetadata.isMovingAroundObstacle) {
            // TODO
            Plans.empty
          } else {
            val (obstacleIndex, segmentIndex) = findNearestObstacle(continuousEnvCell, signalVector)

            if (obstacleIndex != -1) {
              // TODO
              Plans.empty
            } else {
              continuousEnvCell.being = moveBeing(continuousEnvCell, signalVector)

              if (isOnBorder(continuousEnvCell)) {
                val neighbour = getNeighbourInPosition(continuousEnvCell)

                Plans(Map((neighbour, Seq(Plan(
                  Arrive(continuousEnvCell),
                  Leave(continuousEnvCell),
                  Stay(continuousEnvCell)
                )))))
              } else {
                Plans.empty
              }
            }
          }
        } else {
          Plans.empty
        }
    }
    (plans, ContinuousEnvMetrics.empty) // TODO
  }

  private def signalMapToSignalVec(signalMap: SignalMap): SignalVector = {
    signalMap.value
      .map({
        case (direction: GridDirection, signal) => directionSignalToSignalVec(direction, signal)
        case (_: Direction, _) => SignalVector(0d, 0d)
      })
      .foldLeft(SignalVector.zero)(_ + _)
      .normalize
  }

  private def directionSignalToSignalVec(direction: GridDirection, signal: Signal): SignalVector = {
    if (direction.isCardinal) {
      SignalVector(direction.shift._2 * signal.value, -direction.shift._1 * signal.value)
    } else { // is diagonal
      SignalVector(direction.shift._2 * signal.value / math.sqrt(2), -direction.shift._1 * signal.value / math.sqrt(2))
    }
  }

  private def findNearestObstacle(cell: ContinuousEnvCell, signalVector: SignalVector): (Int, Int) = {
    // TODO
    (-1, -1)
  }

  private def moveBeing(cell: ContinuousEnvCell, signalVector: SignalVector): Being = {
    val newX = math.max(cell.cellOutline.x.doubleValue, math.min(cell.being.x + signalVector.x * cell.being.speed, (cell.cellOutline.x + cell.cellOutline.width).doubleValue))
    val newY = math.max(cell.cellOutline.y.doubleValue, math.min(cell.being.y + signalVector.y * cell.being.speed, (cell.cellOutline.y + cell.cellOutline.height).doubleValue))
    Being(newX, newY, cell.being.speed)
  }

  private def isOnBorder(cell: ContinuousEnvCell): Boolean = {
    val leftBorder = cell.cellOutline.x
    val rightBorder = cell.cellOutline.x + cell.cellOutline.width
    val bottomBorder = cell.cellOutline.y
    val topBorder = cell.cellOutline.y + cell.cellOutline.height

    cell.being.x == leftBorder || cell.being.x == rightBorder || cell.being.y == bottomBorder || cell.being.y == topBorder
  }

  private def getNeighbourInPosition(cell: ContinuousEnvCell): GridMultiCellId = {
    val diagonalNeighbour = cell.neighbourhood.diagonalNeighbourhood
      .filter { case (direction, _) => isInPositionDiagonal(direction, cell) }
      .map { case (_, neighbourId) => neighbourId }
      .find(neighbourId => neighbourId != null)

    if (diagonalNeighbour.nonEmpty) {
      diagonalNeighbour.get
    } else {
      cell.neighbourhood.cardinalNeighbourhood
        .filter { case (direction, _) => isInPositionCardinal(direction, cell) }
        .map { case (direction, boundary) => getNeighbourInBoundary(direction, boundary, cell) }
        .head
    }
  }

  private def isInPositionDiagonal(direction: GridDirection, cell: ContinuousEnvCell): Boolean = {
    val cellOutlineLeft = cell.cellOutline.x.doubleValue
    val cellOutlineRight = (cell.cellOutline.x + cell.cellOutline.width).doubleValue
    val cellOutlineBottom = cell.cellOutline.y.doubleValue
    val cellOutlineTop = (cell.cellOutline.y + cell.cellOutline.height).doubleValue
    direction match {
      case TopLeft => cell.being.x == cellOutlineLeft && cell.being.y == cellOutlineTop
      case TopRight => cell.being.x == cellOutlineRight && cell.being.y == cellOutlineTop
      case BottomRight => cell.being.x == cellOutlineRight && cell.being.y == cellOutlineBottom
      case BottomLeft => cell.being.x == cellOutlineLeft && cell.being.y == cellOutlineBottom
      case _ => false
    }
  }

  private def isInPositionCardinal(direction: GridDirection, cell: ContinuousEnvCell): Boolean = {
    val cellOutlineLeft = cell.cellOutline.x.doubleValue
    val cellOutlineRight = (cell.cellOutline.x + cell.cellOutline.width).doubleValue
    val cellOutlineBottom = cell.cellOutline.y.doubleValue
    val cellOutlineTop = (cell.cellOutline.y + cell.cellOutline.height).doubleValue
    direction match {
      case Top => cell.being.y == cellOutlineTop
      case Right => cell.being.x == cellOutlineRight
      case Bottom => cell.being.y == cellOutlineBottom
      case Left => cell.being.x == cellOutlineLeft
      case _ => false
    }
  }

  private def getNeighbourInBoundary(direction: GridDirection, boundary: Boundary, cell: ContinuousEnvCell): GridMultiCellId = {
    direction match {
      case Top | Bottom => getNeighbourInBoundaryPosition(boundary, cell.being.x)
      case Left | Right => getNeighbourInBoundaryPosition(boundary, cell.being.y)
      case _ => null
    }
  }

  private def getNeighbourInBoundaryPosition(boundary: Boundary, position: Double): GridMultiCellId = {
    boundary.boundaries
      .filter { case (segment, _) => containsPosition(segment, position) }
      .map { case (_, neighbourId) => neighbourId }
      .head
  }

  private def containsPosition(segment: Segment, position: Double): Boolean = {
    position >= segment.a.doubleValue && position <= segment.b.doubleValue
  }

  private def getObstacleSegment(cell: ContinuousEnvCell, obstacleIndex: Int, segmentIndex: Int): ObstacleSegment = {
    val obstacle = cell.obstacles(obstacleIndex)
    val a = (obstacle.xs(segmentIndex), obstacle.ys(segmentIndex))
    val b = (obstacle.xs((segmentIndex + 1) % obstacle.points), obstacle.ys((segmentIndex + 1) % obstacle.points))
    ObstacleSegment(a, b)
  }

  private def getMovementDirectionAfterObstacleHit(cell: ContinuousEnvCell, obstacleIndex: Int, segmentIndex: Int, signalVector: SignalVector): MovementDirection = {
    val obstacleSegment = getObstacleSegment(cell, obstacleIndex, segmentIndex)
    val segmentVector = ((obstacleSegment.b._1 - obstacleSegment.a._1).doubleValue, (obstacleSegment.b._2 - obstacleSegment.a._2).doubleValue)
    val directionIndicator = signalVector.x * segmentVector._1 + signalVector.y + segmentVector._2
    if (directionIndicator > 0d) {
      MovementDirection.Clockwise
    } else if (directionIndicator < 0d) {
      MovementDirection.CounterClockwise
    } else if (directionIndicator == 0d) {
      if (random.nextDouble() > 0.5d) {
        MovementDirection.Clockwise
      } else {
        MovementDirection.CounterClockwise
      }
    }

    MovementDirection.Clockwise // should not reach this place
  }
}
