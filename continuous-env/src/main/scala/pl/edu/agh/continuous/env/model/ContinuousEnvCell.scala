package pl.edu.agh.continuous.env.model

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{CellContents, Signal}

final case class ContinuousEnvCell(initialSignal: Signal) extends CellContents {
  override def generateSignal(iteration: Long)(implicit config: XinukConfig): Signal =
    initialSignal

  var visited = false
}
