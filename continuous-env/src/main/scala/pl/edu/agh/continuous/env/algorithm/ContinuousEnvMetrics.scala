package pl.edu.agh.continuous.env.algorithm

import pl.edu.agh.xinuk.algorithm.Metrics

final case class ContinuousEnvMetrics(obstacleHits: Long,
                                      cellTransitions: Double) extends Metrics {

  override def log: String = {
    s"$obstacleHits;$cellTransitions"
  }

  override def series: Vector[(String, Double)] = Vector(
    "Obstacle hits" -> obstacleHits.toDouble,
    "Cell transitions" -> cellTransitions.toDouble
  )

  override def +(other: Metrics): ContinuousEnvMetrics = {
    other match {
      case ContinuousEnvMetrics.Empty => this
      case ContinuousEnvMetrics(otherObstacleHits, otherCellTransitions) =>
        ContinuousEnvMetrics(
          obstacleHits + otherObstacleHits,
          cellTransitions + otherCellTransitions)
      case _ => throw new UnsupportedOperationException(s"Cannot add non-ContinuousEnvMetrics to ContinuousEnvMetrics")
    }
  }
}

object ContinuousEnvMetrics {
  val MetricHeaders = Vector(
    "obstacleHits",
    "cellTransitions"
  )

  private val Empty = ContinuousEnvMetrics(0, 0)
  private val ObstacleHit = ContinuousEnvMetrics(1, 0)
  private val CellTransition = ContinuousEnvMetrics(0, 1)

  def empty: ContinuousEnvMetrics = Empty
  def obstacleHit: ContinuousEnvMetrics = ObstacleHit
  def cellTransition: ContinuousEnvMetrics = CellTransition
}
