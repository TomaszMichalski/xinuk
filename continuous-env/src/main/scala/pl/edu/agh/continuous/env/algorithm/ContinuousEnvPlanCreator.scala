package pl.edu.agh.continuous.env.algorithm

import pl.edu.agh.continuous.env.config.ContinuousEnvConfig
import pl.edu.agh.continuous.env.model.ContinuousEnvCell
import pl.edu.agh.continuous.env.model.continuous.SignalVector
import pl.edu.agh.xinuk.algorithm.{PlanCreator, Plans}
import pl.edu.agh.xinuk.model.continuous.NeighbourhoodState
import pl.edu.agh.xinuk.model.grid.GridDirection
import pl.edu.agh.xinuk.model.{CellId, CellState, Direction, Signal, SignalMap}

final case class ContinuousEnvPlanCreator() extends PlanCreator[ContinuousEnvConfig] {

  override def createPlans(iteration: Long, cellId: CellId, cellState: CellState, neighbourhoodState: NeighbourhoodState)
                          (implicit config: ContinuousEnvConfig): (Plans, ContinuousEnvMetrics) = {
    cellState.contents match {
      case continuousEnvCell: ContinuousEnvCell => processMovement(continuousEnvCell, cellState, neighbourhoodState)
      case _ => (Plans.empty, ContinuousEnvMetrics.empty)
    }
  }

  private def processMovement(continuousEnvCell: ContinuousEnvCell, cellState: CellState, neighbourhoodState: NeighbourhoodState): (Plans, ContinuousEnvMetrics) = {
    if (continuousEnvCell.being == null) {
      (Plans.empty, ContinuousEnvMetrics.empty)
    } else {
      val signalVector = signalMapToSignalVec(cellState.signalMap)
      (Plans.empty, ContinuousEnvMetrics.empty) // TODO
    }
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
}
