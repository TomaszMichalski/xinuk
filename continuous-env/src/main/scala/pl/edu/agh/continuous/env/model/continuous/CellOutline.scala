package pl.edu.agh.continuous.env.model.continuous

import pl.edu.agh.continuous.env.config.ContinuousEnvConfig

case class CellOutline(var x: Double, var y: Double, var width: Double, var height: Double) {

}

object CellOutline {
  def default()(implicit config: ContinuousEnvConfig): CellOutline = CellOutline(0, 0, config.cellSize, config.cellSize)
}
