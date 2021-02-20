package pl.edu.agh.xinuk.model.continuous

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model.CellState

case class Boundary(boundaries: Map[Segment, GridMultiCellId])

object Boundary {
  private val Empty = Boundary(Map.empty)

  def empty(): Boundary = Empty
  def full(cellId: GridMultiCellId)(implicit config: XinukConfig) = Boundary(Map(Segment(0, config.cellSize) -> cellId))
}

case class BoundaryState(boundaryStates: Map[Segment, CellState])

object BoundaryState {
  private val Empty = BoundaryState(Map.empty)

  def empty(): BoundaryState = Empty
  def of(boundaryStates: Map[Segment, CellState]) = BoundaryState(boundaryStates)
}

