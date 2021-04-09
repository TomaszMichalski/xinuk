package pl.edu.agh.continuous.env.algorithm

import pl.edu.agh.continuous.env.algorithm.ContinuousEnvUpdateTag.{Arrive, Leave, Stay}
import pl.edu.agh.continuous.env.config.ContinuousEnvConfig
import pl.edu.agh.continuous.env.model.ContinuousEnvCell
import pl.edu.agh.continuous.env.model.continuous.MovementDecision.{Free, MovementDecision, SlideBackward, SlideForward, Stuck}
import pl.edu.agh.continuous.env.model.continuous.MovementDirection.{Clockwise, CounterClockwise, MovementDirection, None}
import pl.edu.agh.continuous.env.model.continuous.{Being, BeingMetadata, MovementDecision, MovementDirection, MovementVector, Obstacle, ObstacleSegment, SignalVector, Vector}
import pl.edu.agh.xinuk.algorithm.{Plan, PlanCreator, Plans}
import pl.edu.agh.xinuk.model.continuous.{Boundary, GridMultiCellId, NeighbourhoodState, Segment}
import pl.edu.agh.xinuk.model.grid.GridDirection
import pl.edu.agh.xinuk.model.grid.GridDirection.{Bottom, BottomLeft, BottomRight, Left, Right, Top, TopLeft, TopRight}
import pl.edu.agh.xinuk.model.{CellId, CellState, Direction, Signal, SignalMap}

import scala.math.{Pi, atan2}
import scala.util.Random

final case class ContinuousEnvPlanCreator() extends PlanCreator[ContinuousEnvConfig] {

  private val random = new Random(System.nanoTime())
  private val eps = 1e-6

  override def createPlans(iteration: Long, cellId: CellId, cellState: CellState, neighbourhoodState: NeighbourhoodState)
                          (implicit config: ContinuousEnvConfig): (Plans, ContinuousEnvMetrics) = {
    cellState.contents match {
      case continuousEnvCell: ContinuousEnvCell => processMovement(continuousEnvCell, cellState, neighbourhoodState)
      case _ => (Plans.empty, ContinuousEnvMetrics.empty)
    }
  }

  private def processMovement(continuousEnvCell: ContinuousEnvCell, cellState: CellState, neighbourhoodState: NeighbourhoodState)
                             (implicit config: ContinuousEnvConfig): (Plans, ContinuousEnvMetrics) = {
    var plans: Plans = Plans.empty

    if (continuousEnvCell.being != null) {
        // val signalVector = signalMapToSignalVec(cellState.signalMap)
        val signalVector = SignalVector(2.0, -2.0)

        if (signalVector != SignalVector.zero) {
          var movementLeft = signalVector.length * continuousEnvCell.being.speed

          while (movementLeft > eps) {
            val movementVector = getMovementVector(continuousEnvCell.being, signalVector, movementLeft)
            if (continuousEnvCell.beingMetadata.isMovingAroundObstacle) {
              val (movementDecisionAngles, zeroVector) = if (isInBeginningOfObstacleSegment(continuousEnvCell)) {
                getMovementDecisionAnglesBeginning(continuousEnvCell)
              } else {
                getMovementDecisionAnglesMiddle(continuousEnvCell)
              }

              val decisionVector = Vector(signalVector.x, signalVector.y)

              val decision = getDecision(movementDecisionAngles, decisionVector, zeroVector)

              decision match {
                case Free =>
                  continuousEnvCell.beingMetadata = BeingMetadata.initial
                case SlideForward =>
                  val (newBeing, movementLength, movedToObstacleEnd) = moveBeingAlongObstacle(continuousEnvCell, movementVector)
                  continuousEnvCell.being = newBeing
                  if (isOnBorder(continuousEnvCell)) {
                    val neighbour = getNeighbourInPosition(continuousEnvCell)
                    continuousEnvCell.beingMetadata = BeingMetadata.initial

                    plans = Plans(Map((neighbour, Seq(Plan(
                      Arrive(continuousEnvCell),
                      Leave(continuousEnvCell),
                      Stay(continuousEnvCell)
                    )))))
                    movementLeft = 0d
                  } else if (movedToObstacleEnd) {
                    val oldBeingMetadata = continuousEnvCell.beingMetadata
                    val nextObstacleSegmentIndex = getNextObstacleSegmentIndexInDirection(oldBeingMetadata.obstacleSegmentIndex, oldBeingMetadata.movementDirection, oldBeingMetadata.segmentsInObstacle)
                    val newBeingMetadata = BeingMetadata(true, oldBeingMetadata.movementDirection, oldBeingMetadata.obstacleIndex, nextObstacleSegmentIndex, oldBeingMetadata.segmentsInObstacle)
                    continuousEnvCell.beingMetadata = newBeingMetadata
                    movementLeft = movementLeft - movementLength
                  } else {
                    movementLeft = movementLeft - movementVector.length
                  }
                case SlideBackward =>
                  val oldBeingMetadata = continuousEnvCell.beingMetadata
                  val newMovementDirection = getOppositeMovementDirection(oldBeingMetadata.movementDirection)
                  val newBeingMetadata = BeingMetadata(true, newMovementDirection, oldBeingMetadata.obstacleIndex, oldBeingMetadata.obstacleSegmentIndex, oldBeingMetadata.segmentsInObstacle)
                  continuousEnvCell.beingMetadata = newBeingMetadata
                case Stuck =>
                  movementLeft = 0d
              }
            }
            if (!continuousEnvCell.beingMetadata.isMovingAroundObstacle) {
              val (obstacleIndex, segmentIndex, intersectionPoint) = findNearestObstacle(continuousEnvCell, movementVector)
              if (obstacleIndex != -1) {
                val (newBeing, movementLength) = moveBeingToObstacle(continuousEnvCell, intersectionPoint)
                val movementDirection = getMovementDirectionAfterObstacleHit(continuousEnvCell, obstacleIndex, segmentIndex, movementVector)
                val segmentsInObstacle = getSegmentsInObstacle(continuousEnvCell, obstacleIndex)
                val newBeingMetadata = BeingMetadata(true, movementDirection, obstacleIndex, segmentIndex, segmentsInObstacle)
                continuousEnvCell.being = newBeing
                continuousEnvCell.beingMetadata = newBeingMetadata
                movementLeft = movementLeft - movementLength
              } else {
                val (newBeing, movementLength) = moveBeing(continuousEnvCell, movementVector)
                continuousEnvCell.being = newBeing
                movementLeft = movementLeft - movementLength

                if (isOnBorder(continuousEnvCell)) {
                  val neighbour = getNeighbourInPosition(continuousEnvCell)
                  continuousEnvCell.beingMetadata = BeingMetadata.initial

                  plans = Plans(Map((neighbour, Seq(Plan(
                    Arrive(continuousEnvCell),
                    Leave(continuousEnvCell),
                    Stay(continuousEnvCell)
                  )))))
                  movementLeft = 0d
                }
              }
            }
          }
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

  private def isInBeginningOfObstacleSegment(cell: ContinuousEnvCell): Boolean = {
    val obstacleSegment = getObstacleSegment(cell, cell.beingMetadata.obstacleIndex, cell.beingMetadata.obstacleSegmentIndex)
    cell.beingMetadata.movementDirection match {
      case Clockwise => isBeingInPoint(cell.being, obstacleSegment.a)
      case CounterClockwise => isBeingInPoint(cell.being, obstacleSegment.b)
      case _ => false
    }
  }

  private def isBeingInPoint(being: Being, point: (Int, Int)): Boolean = {
    math.abs(being.x - point._1) < eps && math.abs(being.y - point._2) < eps
  }

  private def getMovementDecisionAnglesBeginning(cell: ContinuousEnvCell): (Map[Double, MovementDecision], Vector) = {
    cell.beingMetadata.movementDirection match {
      case Clockwise =>
        val currentSegment = getObstacleSegment(cell, cell.beingMetadata.obstacleIndex, cell.beingMetadata.obstacleSegmentIndex)
        val prevSegment = getObstacleSegment(cell, cell.beingMetadata.obstacleIndex, (cell.beingMetadata.obstacleSegmentIndex + cell.beingMetadata.segmentsInObstacle - 1) % cell.beingMetadata.segmentsInObstacle)

        val currentSegmentVector = toVector(currentSegment.a, currentSegment.b)
        val prevSegmentVector = toVector(prevSegment.b, prevSegment.a)

        val angleCurPrev = atan2Indicator(currentSegmentVector, prevSegmentVector)

        if (angleCurPrev < 180) {
          (Map(
            0d -> Free,
            angleCurPrev -> SlideBackward,
            angleCurPrev + 90d -> Stuck,
            270d -> SlideForward
          ), currentSegmentVector)
        } else {
          (Map(
            0d -> Free,
            angleCurPrev -> SlideBackward,
            180 + angleCurPrev / 2 -> SlideForward
          ), currentSegmentVector)
        }

      case CounterClockwise =>
        val currentSegment = getObstacleSegment(cell, cell.beingMetadata.obstacleIndex, cell.beingMetadata.obstacleSegmentIndex)
        val prevSegment = getObstacleSegment(cell, cell.beingMetadata.obstacleIndex, (cell.beingMetadata.obstacleSegmentIndex + 1) % cell.beingMetadata.segmentsInObstacle)

        val currentSegmentVector = toVector(currentSegment.b, currentSegment.a)
        val prevSegmentVector = toVector(prevSegment.a, prevSegment.b)

        val angleCurPrev = atan2Indicator(currentSegmentVector, prevSegmentVector)

        if (angleCurPrev < 180) {
          (Map(
            0d -> SlideForward,
            angleCurPrev / 2 -> SlideBackward,
            angleCurPrev -> Free
          ), currentSegmentVector)
        } else {
          (Map(
            0d -> SlideForward,
            90d -> Stuck,
            angleCurPrev - 90d -> SlideBackward,
            angleCurPrev -> Free
          ), currentSegmentVector)
        }

      case _ => (Map.empty, Vector.zero)
    }
  }

  private def getMovementDecisionAnglesMiddle(cell: ContinuousEnvCell): (Map[Double, MovementDecision], Vector) = {
    val zeroVector = obstacleSegmentToZeroVector(cell)
    cell.beingMetadata.movementDirection match {
      case Clockwise =>
        (Map(
          0d -> Free,
          180d -> SlideBackward,
          270d -> SlideForward), zeroVector)
      case CounterClockwise =>
        (Map(
          0d -> SlideForward,
          90d -> SlideBackward,
          180d -> Free), zeroVector)
      case _ => (Map.empty, zeroVector)
    }
  }

  private def obstacleSegmentToZeroVector(cell: ContinuousEnvCell): Vector = {
    val currentSegment = getObstacleSegment(cell, cell.beingMetadata.obstacleIndex, cell.beingMetadata.obstacleSegmentIndex)
    cell.beingMetadata.movementDirection match {
      case Clockwise => toVector(currentSegment.a, currentSegment.b)
      case CounterClockwise => toVector(currentSegment.b, currentSegment.a)
      case _ => Vector.zero
    }
  }

  private def getDecision(decisionAngles: Map[Double, MovementDecision], decisionVector: Vector, zeroVector: Vector): MovementDecision = {
    val decisionAngle = atan2Indicator(zeroVector, decisionVector)
    var chosenDecision = Stuck
    for ((decisionThreshold, decision) <- decisionAngles) {
      if (decisionAngle >= decisionThreshold) {
        chosenDecision = decision
      }
    }

    chosenDecision
  }

  private def getOppositeMovementDirection(movementDirection: MovementDirection): MovementDirection = {
    if (movementDirection == Clockwise) {
      CounterClockwise
    } else if (movementDirection == CounterClockwise) {
      Clockwise
    } else {
      None
    }
  }

  private def moveBeingAlongObstacle(cell: ContinuousEnvCell, movementVector: MovementVector): (Being, Double, Boolean) = {
    val obstacleSegmentVector = obstacleSegmentToZeroVector(cell)
    val movementVectorAsVector = toVector(movementVector)
    val factor = dot(movementVectorAsVector, obstacleSegmentVector) / obstacleSegmentVector.lengthSquared
    val newMovementVectorAsVector = Vector(obstacleSegmentVector.x * factor, obstacleSegmentVector.y * factor)
    val newMovementVector = toMovementVector(cell.being, newMovementVectorAsVector)
    val nextVertex = getNextVertex(cell)
    val remainingObstacleVector = getRemainingObstacleMovementVector(cell.being, nextVertex)

    if (remainingObstacleVector.lengthSquared > newMovementVector.lengthSquared) { // being will not reach next vertex
      val (newBeing, movementLength) = moveBeing(cell, newMovementVector)
      (newBeing, movementLength, false)
    } else {
      val (newBeing, movementLength) = moveBeing(cell, remainingObstacleVector)
      (newBeing, movementLength, true)
    }
  }

  private def dot(first: Vector, second: Vector): Double = {
    first.x * second.x + first.y * second.y
  }

  private def toMovementVector(being: Being, vector: Vector): MovementVector = {
    MovementVector((being.x, being.y), (being.x + vector.x, being.y + vector.y))
  }

  private def getNextObstacleSegmentIndexInDirection(obstacleSegmentIndex: Int, movementDirection: MovementDirection, segmentsInObstacle: Int): Int = {
    movementDirection match {
      case Clockwise => (obstacleSegmentIndex + 1) % segmentsInObstacle
      case CounterClockwise => (obstacleSegmentIndex + segmentsInObstacle - 1) % segmentsInObstacle
      case _ => -1
    }
  }

  private def getNextVertex(cell: ContinuousEnvCell): (Double, Double) = {
    val currentSegment = getObstacleSegment(cell, cell.beingMetadata.obstacleIndex, cell.beingMetadata.obstacleSegmentIndex)
    cell.beingMetadata.movementDirection match {
      case Clockwise => (currentSegment.b._1.doubleValue, currentSegment.b._2.doubleValue)
      case CounterClockwise => (currentSegment.a._1.doubleValue, currentSegment.a._2.doubleValue)
      case _ => (0d, 0d)
    }
  }

  private def getRemainingObstacleMovementVector(being: Being, nextVertex: (Double, Double)): MovementVector = {
    MovementVector((being.x, being.y), nextVertex)
  }

  private def toVector(start: (Int, Int), end: (Int, Int)): Vector = {
    Vector(end._1.doubleValue - start._1.doubleValue, end._2.doubleValue - start._2.doubleValue)
  }

  private def toVector(movementVector: MovementVector): Vector = {
    Vector(movementVector.x, movementVector.y)
  }

  private def atan2Indicator(first: Vector, second: Vector): Double = {
    val a = atan2(first.x * second.y - first.y * second.x, first.x * second.x + first.y * second.y) * 180 / Pi
    if (a < 0) {
      360 + a
    } else {
      a
    }
  }

  private def findNearestObstacle(cell: ContinuousEnvCell, movementVector: MovementVector): (Int, Int, (Double, Double)) = {
    cell.obstacles
      .zipWithIndex
      .map { case (obstacle, obstacleIndex) => getClosestSegmentIndex(cell.being, movementVector, obstacle, obstacleIndex) }
      .filter { case (_, segmentIndex, _, _) => segmentIndex != -1 }
      .sortBy(_._4)
      .map { case (obstacleIndex, segmentIndex, intersectionPoint, _) => (obstacleIndex, segmentIndex, intersectionPoint) }
      .headOption
      .getOrElse((-1, -1, (0d, 0d)))
  }

  private def getClosestSegmentIndex(being: Being, movementVector: MovementVector, obstacle: Obstacle, obstacleIndex: Int): (Int, Int, (Double, Double), Double) = {
    val (closestSegmentIndex, intersectionPoint, distance): (Int, (Double, Double), Double) = toObstacleSegments(obstacle)
      .map{ case (segmentIndex, obstacleSegment) => (segmentIndex, obstacleSegment, getIntersectionPoint(movementVector, obstacleSegment)) }
      .filter { case (_, _, intersectionPoint) => intersectionPoint != (-1, -1) }
      .map { case (segmentIndex, _, intersectionPoint) => (segmentIndex, intersectionPoint, getDistance(being, intersectionPoint)) }
      .sortBy(_._3)
      .headOption
      .getOrElse(-1, (-1d, -1d), -1d)

    (obstacleIndex, closestSegmentIndex, intersectionPoint, distance)
  }

  private def toObstacleSegments(obstacle: Obstacle): Array[(Int, ObstacleSegment)] = {
    var obstacleSegments: Array[(Int, ObstacleSegment)] = Array()
    for (i <- 0 until obstacle.points) {
      obstacleSegments = obstacleSegments :+ (i, ObstacleSegment((obstacle.xs(i), obstacle.ys(i)), (obstacle.xs((i + 1) % obstacle.points), obstacle.ys((i + 1) % obstacle.points))))
    }

    obstacleSegments
  }

  private def getIntersectionPoint(movementVector: MovementVector, obstacleSegment: ObstacleSegment): (Double, Double) = {
    val (x1, y1, x2, y2) = (movementVector.a._1, movementVector.a._2, movementVector.b._1, movementVector.b._2)
    val (x3, y3, x4, y4) = (obstacleSegment.a._1, obstacleSegment.a._2, obstacleSegment.b._1, obstacleSegment.b._2)

    val uaNum = (x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)
    val ubNum = (x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3)
    val den = (y4- y3) * (x2 - x1) - (x4 - x3) * (y2 - y1)

    if (math.abs(den) < eps) {
      if (math.abs(uaNum) < eps && math.abs(ubNum) < eps) { // coincident
        Array(obstacleSegment.a, obstacleSegment.b)
          .filter(point => isOnMovementVector(point, movementVector))
          .map(point => (point._1.doubleValue, point._2.doubleValue))
          .map(point => (point, getDistance(point, movementVector.a)))
          .sortBy(_._2)
          .map(_._1)
          .headOption
          .getOrElse((-1d, -1d))
      } else { // parallel
        (-1d, -1d)
      }
    } else {
      val ua = uaNum / den
      val ub = ubNum / den

      if (ua >= 0d && ua <= 1d && ub >= 0d && ub <= 1d) { // intersecting
        (x1 + ua * (x2 - x1), y1 + ua * (y2 - y1))
      } else { // not parallel and not intersecting
        (-1d, -1d)
      }
    }
  }

  private def isOnMovementVector(point: (Int, Int), movementVector: MovementVector): Boolean = {
    isBetweenTwoValues(point._1, movementVector.a._1, movementVector.a._2) &&
      isBetweenTwoValues(point._2, movementVector.b._1, movementVector.b._2)
  }

  private def isBetweenTwoValues(value: Int, a: Double, b: Double): Boolean = {
    if (a < b) {
       value >= a && value <= b
    } else {
      value >= b && value <= a
    }
  }

  private def getDistance(being: Being, point: (Double, Double)): Double = {
    getDistance((being.x, being.y), point)
  }

  private def getDistance(a: (Double, Double), b: (Double, Double)): Double = {
    math.sqrt((a._1 - b._1) * (a._1 - b._1) + (a._2 - b._2) * (a._2 - b._2))
  }

  private def getMovementVector(being: Being, signalVector: SignalVector, movementLeft: Double): MovementVector = {
    val movementStart = (being.x, being.y)
    val movementEnd = (being.x + signalVector.x * movementLeft / signalVector.length, being.y + signalVector.y * movementLeft / signalVector.length)
    MovementVector(movementStart, movementEnd)
  }

  private def moveBeing(cell: ContinuousEnvCell, movementVector: MovementVector): (Being, Double) = {
    val newX = math.max(cell.cellOutline.x.doubleValue, math.min(cell.being.x + movementVector.x, (cell.cellOutline.x + cell.cellOutline.width).doubleValue))
    val newY = math.max(cell.cellOutline.y.doubleValue, math.min(cell.being.y + movementVector.y, (cell.cellOutline.y + cell.cellOutline.height).doubleValue))
    (Being(newX, newY, cell.being.speed), getMovementLength((cell.being.x, cell.being.y), (newX, newY)))
  }

  private def moveBeingToObstacle(cell: ContinuousEnvCell, intersectionPoint: (Double, Double)): (Being, Double) = {
    val newX = intersectionPoint._1
    val newY = intersectionPoint._2
    (Being(newX, newY, cell.being.speed), getMovementLength((cell.being.x, cell.being.y), (newX, newY)))
  }

  private def getMovementLength(start: (Double, Double), end: (Double, Double)): Double = {
    math.sqrt((start._1 - end._1) * (start._1 - end._1) + (start._2 - end._2) * (start._2 - end._2))
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

  private def getMovementDirectionAfterObstacleHit(cell: ContinuousEnvCell, obstacleIndex: Int, segmentIndex: Int, movementVector: MovementVector): MovementDirection = {
    val obstacleSegment = getObstacleSegment(cell, obstacleIndex, segmentIndex)
    val segmentVector = ((obstacleSegment.b._1 - obstacleSegment.a._1).doubleValue, (obstacleSegment.b._2 - obstacleSegment.a._2).doubleValue)
    val directionIndicator = movementVector.x * segmentVector._1 + movementVector.y + segmentVector._2
    if (directionIndicator > 0d) {
      MovementDirection.Clockwise
    } else if (directionIndicator < 0d) {
      MovementDirection.CounterClockwise
    } else {
      if (random.nextDouble() > 0.5d) {
        MovementDirection.Clockwise
      } else {
        MovementDirection.CounterClockwise
      }
    }
  }

  private def getSegmentsInObstacle(cell: ContinuousEnvCell, obstacleIndex: Int): Int = {
    cell.obstacles(obstacleIndex).points
  }
}
