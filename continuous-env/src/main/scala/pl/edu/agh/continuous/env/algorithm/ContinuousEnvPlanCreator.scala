package pl.edu.agh.continuous.env.algorithm

import pl.edu.agh.continuous.env.config.ContinuousEnvConfig
import pl.edu.agh.continuous.env.model.{ContinuousEnvBeing, ContinuousEnvCell}
import pl.edu.agh.xinuk.algorithm.{Plan, PlanCreator, Plans, StateUpdate}
import pl.edu.agh.xinuk.model.{CellContents, CellId, CellState, Direction, Empty, Signal, SignalMap}

import scala.util.Random

final case class ContinuousEnvPlanCreator() extends PlanCreator[ContinuousEnvConfig] {
  var start = false

  override def createPlans(iteration: Long, cellId: CellId, cellState: CellState, neighbourContents: Map[Direction, CellContents])
                          (implicit config: ContinuousEnvConfig): (Plans, ContinuousEnvMetrics) = {
    if (!start) {
      println("Simulation started at " + System.currentTimeMillis())
      start = true
    }

    cellState.contents match {
      case being: ContinuousEnvBeing => (moveBeing(being, cellState.signalMap, neighbourContents), ContinuousEnvMetrics.empty)
      case _ => (Plans.empty, ContinuousEnvMetrics.empty)
    }
  }

  private def moveBeing(being: ContinuousEnvBeing, cellSignal: SignalMap, neighbourContents: Map[Direction, CellContents]): Plans = {
    val directions =
      Random.shuffle(cellSignal.filter { case (direction, _) => neighbourContents.contains(direction) }.toSeq)
        .sortBy(_._2)(Ordering[Signal].reverse)
        .map(_._1)

    if (directions.nonEmpty) {
      Plans(Map((directions.head, Seq(Plan(StateUpdate(being), StateUpdate(ContinuousEnvCell(Signal.zero)))))))
    } else {
      Plans.empty
    }
  }
}
