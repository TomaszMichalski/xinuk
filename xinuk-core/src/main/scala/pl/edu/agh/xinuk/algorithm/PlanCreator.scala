package pl.edu.agh.xinuk.algorithm

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.continuous.NeighbourhoodState

trait PlanCreator[Config <: XinukConfig] {
  def initialize(worldShard: WorldShard)(implicit config: Config): Unit = ()

  def createPlans(iteration: Long, cellId: CellId, cellState: CellState, neighbourhoodState: NeighbourhoodState)
                 (implicit config: Config): (Plans, Metrics)

  def finalize(worldShard: WorldShard)(implicit config: Config): Unit = ()
}
