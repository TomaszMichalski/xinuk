package pl.edu.agh.xinuk.model.grid

import pl.edu.agh.xinuk.model.continuous.{GridMultiCellId, Segment}

case class CardinalNeighbourhoodUpdate(newCellId: GridMultiCellId, dividedCellId: GridMultiCellId, newBoundaryItems: Map[Segment, GridMultiCellId])
