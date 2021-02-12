package pl.edu.agh.continuous.env.algorithm

import pl.edu.agh.continuous.env.config.ContinuousEnvConfig
import pl.edu.agh.xinuk.algorithm.{PlanCreator, Plans}
import pl.edu.agh.xinuk.model.{CellContents, CellId, CellState, Direction}

final case class ContinuousEnvPlanCreator() extends PlanCreator[ContinuousEnvConfig] {

  override def createPlans(iteration: Long, cellId: CellId, cellState: CellState, neighbourContents: Map[Direction, CellContents])
                          (implicit config: ContinuousEnvConfig): (Plans, ContinuousEnvMetrics) = {
    cellState.contents match {
      case _ => (Plans.empty, ContinuousEnvMetrics.empty)
    }
  }
}
