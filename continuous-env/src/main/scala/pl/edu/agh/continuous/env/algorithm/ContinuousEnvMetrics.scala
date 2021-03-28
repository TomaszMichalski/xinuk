package pl.edu.agh.continuous.env.algorithm

import pl.edu.agh.continuous.env.algorithm.ContinuousEnvMetrics.cellTransition
import pl.edu.agh.xinuk.algorithm.Metrics

final case class ContinuousEnvMetrics(obstacleHits: Long,
                                      cellTransitions: Long,
                                      sourceReached: Long) extends Metrics {

  override def log: String = {
    s"$obstacleHits;$cellTransitions;$sourceReached"
  }

  override def series: Vector[(String, Double)] = Vector(
    "Obstacle hits" -> obstacleHits.toDouble,
    "Cell transitions" -> cellTransitions.toDouble,
    "Source reached" -> sourceReached.toDouble
  )

  override def +(other: Metrics): ContinuousEnvMetrics = {
    other match {
      case ContinuousEnvMetrics(otherObstacleHits, otherCellTransitions, otherSourceReached) =>
        ContinuousEnvMetrics(
          obstacleHits + otherObstacleHits,
          cellTransitions + otherCellTransitions,
          sourceReached + otherSourceReached)
      case _ => throw new UnsupportedOperationException(s"Cannot add non-ContinuousEnvMetrics to ContinuousEnvMetrics")
    }
  }
}

object ContinuousEnvMetrics {
  val MetricHeaders = Vector(
    "obstacleHits",
    "cellTransitions",
    "sourceReached"
  )

  private val Empty = ContinuousEnvMetrics(0, 0, 0)
  private val ObstacleHit = ContinuousEnvMetrics(1, 0, 0)
  private val CellTransition = ContinuousEnvMetrics(0, 1, 0)
  private val SourceReached = ContinuousEnvMetrics(0, 0, 1)

  def empty: ContinuousEnvMetrics = Empty
  def obstacleHit: ContinuousEnvMetrics = ObstacleHit
  def cellTransition: ContinuousEnvMetrics = CellTransition
  def sourceReached: ContinuousEnvMetrics = SourceReached
}
