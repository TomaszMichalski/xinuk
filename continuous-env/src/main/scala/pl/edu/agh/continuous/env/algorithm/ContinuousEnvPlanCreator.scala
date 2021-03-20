package pl.edu.agh.continuous.env.algorithm

import pl.edu.agh.continuous.env.config.ContinuousEnvConfig
import pl.edu.agh.continuous.env.model.ContinuousEnvCell
import pl.edu.agh.xinuk.algorithm.{PlanCreator, Plans}
import pl.edu.agh.xinuk.model.continuous.NeighbourhoodState
import pl.edu.agh.xinuk.model.{CellId, CellState}

final case class ContinuousEnvPlanCreator() extends PlanCreator[ContinuousEnvConfig] {

  override def createPlans(iteration: Long, cellId: CellId, cellState: CellState, neighbourhoodState: NeighbourhoodState)
                          (implicit config: ContinuousEnvConfig): (Plans, ContinuousEnvMetrics) = {
    cellState.contents match {
      case continuousEnvCell: ContinuousEnvCell => processMovement(continuousEnvCell, neighbourhoodState)
      case _ => (Plans.empty, ContinuousEnvMetrics.empty)
    }
  }

  private def processMovement(continuousEnvCell: ContinuousEnvCell, neighbourhoodState: NeighbourhoodState): (Plans, ContinuousEnvMetrics) = {
    if (continuousEnvCell.being == null) {
      (Plans.empty, ContinuousEnvMetrics.empty)
    } else {
      (Plans.empty, ContinuousEnvMetrics.empty) // TODO
    }
  }
}
