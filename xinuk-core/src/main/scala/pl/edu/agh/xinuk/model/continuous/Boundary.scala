package pl.edu.agh.xinuk.model.continuous

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.CellId

case class Boundary(boundaries: Map[Segment, CellId])

object Boundary {
  private val Empty = Boundary(Map.empty)

  def empty(): Boundary = Empty
  def full(cellId: CellId)(implicit config: XinukConfig) = Boundary(Map(Segment(0, config.cellSize) -> cellId))
}
