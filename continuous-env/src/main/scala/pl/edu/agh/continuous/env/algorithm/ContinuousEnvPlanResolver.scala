package pl.edu.agh.continuous.env.algorithm

import pl.edu.agh.continuous.env.algorithm.ContinuousEnvUpdateTag.{Arrive, Leave, Stay}
import pl.edu.agh.continuous.env.config.ContinuousEnvConfig
import pl.edu.agh.continuous.env.model.ContinuousEnvCell
import pl.edu.agh.continuous.env.model.continuous.{Being, BeingMetadata}
import pl.edu.agh.xinuk.algorithm.{PlanResolver, StateUpdate}
import pl.edu.agh.xinuk.model.{CellContents, Signal}

final case class ContinuousEnvPlanResolver() extends PlanResolver[ContinuousEnvConfig] {

  private val eps = 1e-6

  override def isUpdateValid(contents: CellContents, update: StateUpdate)(implicit config: ContinuousEnvConfig): Boolean =
    (contents, update.updateTag, update.value) match {
      case (_: ContinuousEnvCell, Leave, _: ContinuousEnvCell) => true // being can leave cell
      case (_: ContinuousEnvCell, Arrive, _: ContinuousEnvCell) => true // being can arrive at cell
      case (_: ContinuousEnvCell, Stay, _: ContinuousEnvCell) => true // being can stay at cell
      case _ => false // nothing else is allowed
    }

  override def applyUpdate(contents: CellContents, update: StateUpdate)(implicit config: ContinuousEnvConfig): (CellContents, ContinuousEnvMetrics) = {
    val (newContents: CellContents, metrics: ContinuousEnvMetrics) = (contents, update.updateTag, update.value) match {
      case (_: ContinuousEnvCell, Leave, oldCell: ContinuousEnvCell) =>
        oldCell.being = null
        oldCell.beingMetadata = BeingMetadata.initial
        (oldCell, ContinuousEnvMetrics.empty)
      case (newCell: ContinuousEnvCell, Arrive, oldCell: ContinuousEnvCell) =>
        if (newCell.initialSignal == Signal.zero) {
          newCell.being = translateBeing(oldCell.being)
          newCell.visited = true
          (newCell, ContinuousEnvMetrics.cellTransition)
        } else {
          (newCell, ContinuousEnvMetrics.sourceReached)
        }
      case (_: ContinuousEnvCell, Stay, oldCell: ContinuousEnvCell) =>
        (oldCell, ContinuousEnvMetrics.empty)

      case _ => throw new IllegalArgumentException(s"Illegal update applied: contents = $contents, update = $update")    }

    (newContents, metrics)
  }

  private def translateBeing(being: Being): Being = {
    Being(translateCoord(being.x), translateCoord(being.y), being.speed)
  }

  private def translateCoord(coord: Double): Double = {
    if (math.abs(coord - 100d) < eps) {
      0d
    } else if (math.abs(coord) < eps) {
      100d
    } else {
      coord
    }
  }
}
