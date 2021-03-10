package pl.edu.agh.continuous.env.model

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.{CellContents, Signal}

final case class ContinuousEnvCell(initialSignal: Signal, signalFactor: Double) extends CellContents {
  override def generateSignal(iteration: Long)(implicit config: XinukConfig): Signal =
    initialSignal

  override def signalFactor(iteration: Long)(implicit config: XinukConfig): Double =
    signalFactor
}
