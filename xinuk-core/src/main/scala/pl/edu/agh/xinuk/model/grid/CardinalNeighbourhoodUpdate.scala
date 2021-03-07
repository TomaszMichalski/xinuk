package pl.edu.agh.xinuk.model.grid

import pl.edu.agh.xinuk.model.continuous.{Boundary, GridMultiCellId}

case class CardinalNeighbourhoodUpdate(newCellId: GridMultiCellId, dividedCellId: GridMultiCellId, newCellBoundary: Boundary)
