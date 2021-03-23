package pl.edu.agh.continuous.env.model

import pl.edu.agh.continuous.env.config.ContinuousEnvConfig
import pl.edu.agh.continuous.env.model.continuous.{Being, BeingMetadata, CellOutline, Obstacle}
import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.continuous.Neighbourhood
import pl.edu.agh.xinuk.model.{CellContents, Signal}

final case class ContinuousEnvCell(initialSignal: Signal)(implicit config: ContinuousEnvConfig) extends CellContents {
  override def generateSignal(iteration: Long)(implicit config: XinukConfig): Signal =
    initialSignal

  var cellOutline: CellOutline = CellOutline.default()
  var neighbourhood: Neighbourhood = Neighbourhood.empty()
  var obstacles: Array[Obstacle] = Array()
  var being: Being = _
  var beingMetadata: BeingMetadata = BeingMetadata.initial
}
