package pl.edu.agh.continuous.env

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.continuous.env.algorithm.{ContinuousEnvMetrics, ContinuousEnvPlanCreator, ContinuousEnvWorldCreator}
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.CellState
import pl.edu.agh.xinuk.model.grid.GridSignalPropagation

import java.awt.Color

object ContinuousEnvMain extends LazyLogging {
  private val configPrefix = "continuous-env"

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation(
      configPrefix,
      ContinuousEnvMetrics,
      ContinuousEnvWorldCreator,
      ContinuousEnvPlanCreator,
      ContinuousEnvPlanResolver,
      ContinuousEnvMetrics.empty,
      GridSignalPropagation.Standard,
      cellToColor
    ).start()
  }

  private def cellToColor: PartialFunction[CellState, Color] = {
    case cellState =>
      cellState.contents match {
        case _ => Color.WHITE
      }
  }
}
