package pl.edu.agh.xinuk.model.continuous

import pl.edu.agh.xinuk.model.CellId
import pl.edu.agh.xinuk.model.grid.GridDirection
import pl.edu.agh.xinuk.model.grid.GridDirection.{Bottom, BottomLeft, BottomRight, Left, Right, Top, TopLeft, TopRight}

case class Neighbourhood(cardinalNeighbourhood: Map[GridDirection, Boundary],
                         diagonalNeighbourhood: Map[GridDirection, CellId])

object Neighbourhood {
  private val Empty = Neighbourhood(
    Map(
      Top -> Boundary.empty(),
      Right -> Boundary.empty(),
      Bottom -> Boundary.empty(),
      Left -> Boundary.empty()
    ),
    Map(
      TopLeft -> null,
      TopRight -> null,
      BottomRight -> null,
      BottomLeft -> null
    ))

  def empty(): Neighbourhood = Empty
}
