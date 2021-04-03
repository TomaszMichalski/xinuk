package pl.edu.agh.continuous.env.model.continuous

import pl.edu.agh.continuous.env.model.continuous.MovementDirection.MovementDirection

case class BeingMetadata(isMovingAroundObstacle: Boolean,
                         movementDirection: MovementDirection,
                         obstacleIndex: Int,
                         obstacleSegmentIndex: Int,
                         segmentsInObstacle: Int)

object BeingMetadata {
  def initial: BeingMetadata = BeingMetadata(false, MovementDirection.None, -1, -1, 0)
}


