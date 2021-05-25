package pl.edu.agh.continuous.env.algorithm

import pl.edu.agh.continuous.env.config.ContinuousEnvConfig
import pl.edu.agh.continuous.env.model.{ContinuousEnvBeing, ContinuousEnvCell}
import pl.edu.agh.xinuk.algorithm.{PlanResolver, StateUpdate}
import pl.edu.agh.xinuk.model.CellContents

final case class ContinuousEnvPlanResolver() extends PlanResolver[ContinuousEnvConfig] {
  override def isUpdateValid(contents: CellContents, update: StateUpdate)(implicit config: ContinuousEnvConfig): Boolean =
    (contents, update.value) match {
      case (_, _: ContinuousEnvCell) => true
      case (_: ContinuousEnvCell, _: ContinuousEnvBeing) => true
      case _ => false                               // nothing else is allowed
    }

  override def applyUpdate(contents: CellContents, update: StateUpdate)(implicit config: ContinuousEnvConfig): (CellContents, ContinuousEnvMetrics) = {
    (contents, update.value) match {
      case (_, cell: ContinuousEnvCell) => {
        cell.visited = true
        (cell, ContinuousEnvMetrics.empty)
      }
      case (cell: ContinuousEnvCell, being: ContinuousEnvBeing) => {
        if (cell.initialSignal.value > 0) {
          println("Simulation finished at " + System.currentTimeMillis())
          (cell, ContinuousEnvMetrics.empty)
        } else {
          (being, ContinuousEnvMetrics.empty)
        }
      }
      case _ => (contents, ContinuousEnvMetrics.empty)
    }
  }
}
