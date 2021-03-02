package pl.edu.agh.continuous.env.model.continuous

import pl.edu.agh.continuous.env.config.ContinuousEnvConfig

case class CellOutline(var x: Int, var y: Int, var width: Int, var height: Int) {

}

object CellOutline {
  def default()(implicit config: ContinuousEnvConfig): CellOutline = CellOutline(0, 0, config.cellSize, config.cellSize)
}
