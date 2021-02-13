package pl.edu.agh.continuous.env.algorithm

import pl.edu.agh.continuous.env.config.ContinuousEnvConfig
import pl.edu.agh.xinuk.algorithm.{PlanResolver, StateUpdate}
import pl.edu.agh.xinuk.model.CellContents

final case class ContinuousEnvPlanResolver() extends PlanResolver[ContinuousEnvConfig] {
  override def isUpdateValid(contents: CellContents, update: StateUpdate)(implicit config: ContinuousEnvConfig): Boolean =
    (contents, update.updateTag, update.value) match {
      case _ => false                               // nothing is allowed
    }

  override def applyUpdate(contents: CellContents, update: StateUpdate)(implicit config: ContinuousEnvConfig): (CellContents, ContinuousEnvMetrics) = {
    (contents, ContinuousEnvMetrics.empty)
  }
}
