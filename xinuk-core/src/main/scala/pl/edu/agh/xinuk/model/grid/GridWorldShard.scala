package pl.edu.agh.xinuk.model.grid

import pl.edu.agh.xinuk.config.XinukConfig
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.continuous.{Boundary, GridMultiCellId, Neighbourhood, Segment}

object GridWorldType extends WorldType {
  override def directions: Seq[Direction] = GridDirection.values
}

final class GridWorldShard(val cells: Map[CellId, Cell],
                           val cellNeighbours: Map[CellId, Neighbourhood],
                           val workerId: WorkerId,
                           val outgoingCells: Map[WorkerId, Set[CellId]],
                           val incomingCells: Map[WorkerId, Set[CellId]],
                           val cellToWorker: Map[CellId, WorkerId])(implicit config: XinukConfig) extends WorldShard {

  private val localCellIdsSet: Set[CellId] = cells.keys.filter(k => cellToWorker(k) == workerId).toSet

  def span: ((Int, Int), (Int, Int)) = {
    val coords = localCellIds.map {
      case GridCellId(x, y) => (x, y)
      case GridMultiCellId(x, y, _) => (x, y)
    }
    val xMin = coords.map(_._1).min
    val xMax = coords.map(_._1).max
    val xSize = xMax - xMin + 1
    val yMin = coords.map(_._2).min
    val yMax = coords.map(_._2).max
    val ySize = yMax - yMin + 1
    ((xMin, yMin), (xSize, ySize))
  }

  override def localCellIds: Set[CellId] = localCellIdsSet
}

object GridWorldShard {
  def apply(cells: Map[CellId, Cell],
            cellNeighbours: Map[CellId, Neighbourhood],
            workerId: WorkerId,
            outgoingCells: Map[WorkerId, Set[CellId]],
            incomingCells: Map[WorkerId, Set[CellId]],
            cellToWorker: Map[CellId, WorkerId])(implicit config: XinukConfig): GridWorldShard =
    new GridWorldShard(cells, cellNeighbours, workerId, outgoingCells, incomingCells, cellToWorker)(config)
}

case class GridWorldBuilder()(implicit config: XinukConfig) extends WorldBuilder {

  import scala.collection.mutable.{Map => MutableMap}

  private val cellsMutable: MutableMap[CellId, Cell] = MutableMap.empty.withDefault(id => Cell.empty(id))
  private val neighboursMutable: MutableMap[CellId, MutableMap[Direction, CellId]] = MutableMap.empty.withDefault(_ => MutableMap.empty)

  private val multiConnectionNeighbours: MutableMap[GridMultiCellId, Neighbourhood] = MutableMap.empty.withDefault(_ => Neighbourhood.empty())
  private val gridMultiCellIdMap: MutableMap[GridCellId, List[GridMultiCellId]] = MutableMap.empty.withDefault(_ => List.empty)

  override def apply(cellId: CellId): Cell = cellsMutable(cellId)

  override def update(cellId: CellId, cellState: CellState): Unit = cellsMutable(cellId) = Cell(cellId, cellState)

  def getExistingNeighbourhood(gridMultiCellId: GridMultiCellId): Neighbourhood = {
    multiConnectionNeighbours(gridMultiCellId)
  }
  def updateNeighbourhoodAfterDividingCell(dividedCellId: GridMultiCellId, newCellNeighbourhoodMap: Map[GridMultiCellId, Neighbourhood]): Unit = {
    updateGridMultiCellIdMap(dividedCellId, newCellNeighbourhoodMap)

    val dividedCellNeighbourhood = multiConnectionNeighbours(dividedCellId)

    dividedCellNeighbourhood.cardinalNeighbourhood
      .foreach { case (direction, boundary) => updateCardinalNeighbourhood(direction, boundary, dividedCellId, newCellNeighbourhoodMap) }

    dividedCellNeighbourhood.diagonalNeighbourhood
      .filter { case (_, neighbourId) => neighbourId != null }
      .foreach { case (direction, neighbourId) => updateDiagonalNeighbourhood(direction, neighbourId, newCellNeighbourhoodMap) }

    updateMultiConnectionNeighbours(dividedCellId, newCellNeighbourhoodMap)
  }

  private def updateGridMultiCellIdMap(dividedCellId: GridMultiCellId, newCellNeighbourhoodMap: Map[GridMultiCellId, Neighbourhood]): Unit = {
    val gridCellId = GridCellId(dividedCellId.x, dividedCellId.y)
    gridMultiCellIdMap(gridCellId) = gridMultiCellIdMap(gridCellId).filter(id => id != dividedCellId) :++ newCellNeighbourhoodMap.keys
  }

  private def updateMultiConnectionNeighbours(dividedCellId: GridMultiCellId, newCellNeighbourhoodMap: Map[GridMultiCellId, Neighbourhood]): Unit = {
    multiConnectionNeighbours.remove(dividedCellId)
    newCellNeighbourhoodMap
      .foreach { case (cellId, neighbourhood) => multiConnectionNeighbours(cellId) = neighbourhood }
  }

  def updateNeighbourhood(cellId: GridMultiCellId, neighbourhood: Neighbourhood): Unit = {
    neighbourhood.cardinalNeighbourhood
      .foreach { case (direction, boundary) => updateCardinalNeighbourhood(direction, boundary, cellId) }

    neighbourhood.diagonalNeighbourhood
      .filter { case (_, neighbourId) => neighbourId != null }
      .foreach { case (direction, neighbourId) => updateDiagonalNeighbourhood(direction, neighbourId, cellId) }

    multiConnectionNeighbours(cellId) = neighbourhood

    val i = 0
  }

  private def updateCardinalNeighbourhood(direction: GridDirection, boundary: Boundary, cellId: GridMultiCellId): Unit = {
    val cardinalNeighbourhoodUpdate = CardinalNeighbourhoodUpdate(cellId, cellId, boundary)
    getNeighboursInBoundary(boundary)
      .foreach(neighbourId => processCardinalNeighbourhoodUpdates(neighbourId, Array(cardinalNeighbourhoodUpdate), direction))
  }

  private def updateDiagonalNeighbourhood(direction: GridDirection, neighbourId: GridMultiCellId, cellId: GridMultiCellId): Unit = {
    val neighbourDiagonalNeighbourhood = MutableMap.from(multiConnectionNeighbours(neighbourId).diagonalNeighbourhood)
    neighbourDiagonalNeighbourhood(direction.opposite) = cellId
    multiConnectionNeighbours(neighbourId) = Neighbourhood(
      multiConnectionNeighbours(neighbourId).cardinalNeighbourhood,
      Map.from(neighbourDiagonalNeighbourhood))
  }

  private def updateCardinalNeighbourhood(direction: GridDirection, boundary: Boundary, dividedCellId: GridMultiCellId, newCellNeighbourhoodMap: Map[GridMultiCellId, Neighbourhood]): Unit = {
    val newCellsWithGivenCardinalNeighbourhood = getNewCellsWithGivenCardinalNeighbourhood(direction, newCellNeighbourhoodMap)
    val cardinalNeighbourhoodUpdates = getCardinalNeighbourhoodUpdates(dividedCellId, newCellsWithGivenCardinalNeighbourhood)

    getNeighboursInBoundary(boundary)
      .foreach(neighbourId => processCardinalNeighbourhoodUpdates(neighbourId, cardinalNeighbourhoodUpdates, direction))
  }

  private def getNewCellsWithGivenCardinalNeighbourhood(direction: GridDirection, newCellNeighbourhoodMap: Map[GridMultiCellId, Neighbourhood]): Map[GridMultiCellId, Boundary] = {
    newCellNeighbourhoodMap
      .map { case (cellId, neighbourhood) => (cellId, neighbourhood.cardinalNeighbourhood(direction)) }
      .filter { case (_, boundary) => boundary.boundaries != Map.empty }
  }

  private def getCardinalNeighbourhoodUpdates(dividedCellId: GridMultiCellId, newCellsWithGivenCardinalNeighbourhood: Map[GridMultiCellId, Boundary]): Array[CardinalNeighbourhoodUpdate] = {
    newCellsWithGivenCardinalNeighbourhood
      .map { case (cellId, boundary) => CardinalNeighbourhoodUpdate(cellId, dividedCellId, boundary) }
      .toArray
  }

  private def getNeighboursInBoundary(boundary: Boundary): Array[GridMultiCellId] = {
    boundary.boundaries
      .map { case (_, neighbourId) => neighbourId }
      .toArray
  }

  private def processCardinalNeighbourhoodUpdates(neighbourId: GridMultiCellId, cardinalNeighbourhoodUpdates: Array[CardinalNeighbourhoodUpdate], direction: GridDirection): Unit = {
    var neighbourBoundaries: MutableMap[Segment, GridMultiCellId] = MutableMap.from(multiConnectionNeighbours(neighbourId).cardinalNeighbourhood(direction.opposite).boundaries)
    for (update <- cardinalNeighbourhoodUpdates) {
      neighbourBoundaries = processCardinalNeighbourhoodUpdate(neighbourId, neighbourBoundaries, update)
    }

    val newNeighbourCardinalNeighbourhood = MutableMap.from(multiConnectionNeighbours(neighbourId).cardinalNeighbourhood)
    newNeighbourCardinalNeighbourhood(direction.opposite) = Boundary(Map.from(neighbourBoundaries))
    multiConnectionNeighbours(neighbourId) = Neighbourhood(
      Map.from(newNeighbourCardinalNeighbourhood),
      multiConnectionNeighbours(neighbourId).diagonalNeighbourhood)
  }

  private def processCardinalNeighbourhoodUpdate(neighbourId: GridMultiCellId, neighbourBoundaries: MutableMap[Segment, GridMultiCellId], update: CardinalNeighbourhoodUpdate): MutableMap[Segment, GridMultiCellId] = {
    val result = neighbourBoundaries
      .filter { case (_, cellId) => cellId != update.dividedCellId } // filter divided cell id segments

    update.newCellBoundary.boundaries
      .filter { case (_, cellId) => cellId == neighbourId } // get segments targeting this neighbour
      .foreach { case (segment, _) => result(segment) = update.newCellId } // set new segment targeting new cell

    result
  }

  private def updateDiagonalNeighbourhood(direction: GridDirection, neighbourId: GridMultiCellId, newCellNeighbourhoodMap: Map[GridMultiCellId, Neighbourhood]): Unit = {
    val newCellWithGivenDiagonalNeighbourhood = getNewCellWithGivenDiagonalNeighbourhood(direction, newCellNeighbourhoodMap)
    val neighbourDiagonalNeighbourhood = MutableMap.from(multiConnectionNeighbours(neighbourId).diagonalNeighbourhood)
    neighbourDiagonalNeighbourhood(direction.opposite) = newCellWithGivenDiagonalNeighbourhood
    multiConnectionNeighbours(neighbourId) = Neighbourhood(
      multiConnectionNeighbours(neighbourId).cardinalNeighbourhood,
      Map.from(neighbourDiagonalNeighbourhood))
  }

  private def getNewCellWithGivenDiagonalNeighbourhood(gridDirection: GridDirection, newCellNeighbourhoodMap: Map[GridMultiCellId, Neighbourhood]): GridMultiCellId = {
    newCellNeighbourhoodMap
      .filter { case (_, neighbourhood) => neighbourhood.diagonalNeighbourhood(gridDirection) != null }
      .map { case (newCellId, _) => newCellId }
      .headOption.orNull
  }

  def withWrappedBoundaries(): GridWorldBuilder = {
    def wrapped(cellId: GridCellId) = GridCellId(Math.floorMod(cellId.x, xSize), Math.floorMod(cellId.y, ySize))

    val boundary: Set[GridCellId] = Seq(
      (0 until xSize).map(x => GridCellId(x, 0)),
      (0 until xSize).map(x => GridCellId(x, ySize - 1)),
      (0 until ySize).map(y => GridCellId(0, y)),
      (0 until ySize).map(y => GridCellId(xSize - 1, y))).flatten.toSet

    for {
      from <- boundary
      direction <- GridDirection.values
      to = direction.of(from)
      if !valid(to)
    } connectOneWay(from, direction, wrapped(to))

    this
  }

  private def valid(cellId: GridCellId): Boolean = cellId.x >= 0 && cellId.x < xSize && cellId.y >= 0 && cellId.y < ySize

  private def valid(cellId: GridMultiCellId): Boolean = cellId.x >= 0 && cellId.x < xSize && cellId.y >= 0 && cellId.y < ySize

  private def ySize: Int = config.worldHeight

  private def xSize: Int = config.worldWidth

  override def connectOneWay(from: CellId, direction: Direction, to: CellId): Unit = {
    val cellNeighbours = neighboursMutable(from)
    cellNeighbours(direction) = to
    neighboursMutable(from) = cellNeighbours
  }

  def multiConnectOneWay(from: GridMultiCellId, direction: GridDirection, to: GridMultiCellId): Unit = {
    val existingNeighbourhood = multiConnectionNeighbours(from)
    direction match {
      case cardinal@(GridDirection.Top | GridDirection.Right | GridDirection.Bottom | GridDirection.Left) => {
        var cardinalNeighbourhood: Map[GridDirection, Boundary] = existingNeighbourhood.cardinalNeighbourhood
        cardinalNeighbourhood += (cardinal -> Boundary.full(to))
        multiConnectionNeighbours += (from -> Neighbourhood(cardinalNeighbourhood, existingNeighbourhood.diagonalNeighbourhood))
      }
      case diagonal@(GridDirection.TopLeft | GridDirection.TopRight | GridDirection.BottomRight | GridDirection.BottomLeft) => {
        var diagonalNeighbourhood: Map[GridDirection, GridMultiCellId] = existingNeighbourhood.diagonalNeighbourhood
        diagonalNeighbourhood += (diagonal -> to)
        multiConnectionNeighbours += (from -> Neighbourhood(existingNeighbourhood.cardinalNeighbourhood, diagonalNeighbourhood))
      }
      case _ =>
    }
  }

  def putToGridCellIdMap(gridMultiCellId: GridMultiCellId): Unit = {
    val gridCellId = GridCellId(gridMultiCellId.x, gridMultiCellId.y)
    var gridMultiCellIds: List[GridMultiCellId] = gridMultiCellIdMap(gridCellId)
    gridMultiCellIds = gridMultiCellIds :+ gridMultiCellId
    gridMultiCellIdMap(gridCellId) = gridMultiCellIds
  }

  def withGridConnections(): GridWorldBuilder = {
    for {
      x <- 0 until xSize
      y <- 0 until ySize
      direction <- GridDirection.values
      from = GridMultiCellId(x, y, 0)
      to = direction.of(from)
      if valid(to)
    } connectOneWay(from, direction, to)

    for {
      x <- 0 until xSize
      y <- 0 until ySize
      direction <- GridDirection.values
      from = GridMultiCellId(x, y, 0)
      to = direction.of(from)
      if valid(to)
    } multiConnectOneWay(from, direction, to)

    for {
      gridMultiCellId <- multiConnectionNeighbours.keys
    } putToGridCellIdMap(gridMultiCellId)

    this
  }

  def build(): Map[WorkerId, GridWorldShard] = {
    val workerDomains = divide()

    val globalCellToWorker: Map[CellId, WorkerId] = workerDomains.flatMap {
      case (workerId, (localIds, _)) => localIds.map { cellId => (cellId, workerId) }
    }

    val globalOutgoingCells: Map[WorkerId, Map[WorkerId, Set[CellId]]] = workerDomains.map {
      case (workerId, (_, remoteIds)) => (workerId, remoteIds.groupBy(globalCellToWorker))
    }

    val globalIncomingCells: Map[WorkerId, Map[WorkerId, Set[CellId]]] = workerDomains.keys.map {
      id => (id, globalOutgoingCells
        .filter { case (_, outgoing) => outgoing.contains(id)}
        .map( { case (otherId, outgoing) => (otherId, outgoing(id)) }))
    }.toMap

    workerDomains.map({ case (workerId, (localIds, remoteIds)) =>

      val cells = (localIds ++ remoteIds).map { id => (id, cellsMutable(id)) }.toMap

      val neighboursOfLocal = multiConnectionNeighbours
        .filter { case (id, _) => localIds.contains(id) }
        .map { case (id, neighbourhood) => (id.asInstanceOf[CellId], neighbourhood) }
        .toMap

      val neighboursOfRemote = multiConnectionNeighbours
        .filter { case (id, _) => remoteIds.contains(id) }
        .map { case (id, neighbourhood) => (id.asInstanceOf[CellId], cutNeighbourhoodOfRemote(neighbourhood, localIds)) }
        .toMap

      val neighbours = neighboursOfLocal ++ neighboursOfRemote

      val outgoingCells = globalOutgoingCells(workerId)

      val incomingCells = globalIncomingCells(workerId)

      val cellToWorker = globalCellToWorker.filter({ case (id, _) => localIds.contains(id) || remoteIds.contains(id) })

      (workerId, GridWorldShard(cells, neighbours, workerId, outgoingCells, incomingCells, cellToWorker))
    })
  }

  private def cutNeighbourhoodOfRemote(neighbourhood: Neighbourhood, localIds: Set[CellId]): Neighbourhood = {
    val cutCardinalNeighbourhood: Map[GridDirection, Boundary] = neighbourhood.cardinalNeighbourhood
      .map { case (gridDirection, boundary) => (gridDirection, cutBoundaryOfRemote(boundary, localIds)) }
    val cutDiagonalNeighbourhood: Map[GridDirection, GridMultiCellId] = neighbourhood.diagonalNeighbourhood
      .filter { case (_, cellId) => localIds.contains(cellId) }
    Neighbourhood(cutCardinalNeighbourhood, cutDiagonalNeighbourhood)
  }

  private def cutBoundaryOfRemote(boundary: Boundary, localIds: Set[CellId]): Boundary = {
    val cutBoundary = boundary.boundaries
      .filter { case (_, cellId) => localIds.contains(cellId) }
    Boundary(cutBoundary)
  }

  private def divide(): Map[WorkerId, (Set[CellId], Set[CellId])] = {
    val xWorkerCount = config.workersRoot
    val yWorkerCount = config.workersRoot

    val xSizes = split(xSize, xWorkerCount)
    val ySizes = split(ySize, yWorkerCount)

    val xOffsets = xSizes.scan(0) { case (acc, value) => acc + value }
    val yOffsets = ySizes.scan(0) { case (acc, value) => acc + value }

    val workerIds: Seq[WorkerId] = (1 to (xWorkerCount * yWorkerCount)).map(WorkerId)

    val workerSpans: Map[WorkerId, ((Int, Int), (Int, Int))] = workerIds.map { workerId =>
      val xPos = (workerId.value - 1) / yWorkerCount
      val yPos = (workerId.value - 1) % yWorkerCount
      val xOffset = xOffsets(xPos)
      val xSize = xSizes(xPos)
      val yOffset = yOffsets(yPos)
      val ySize = ySizes(yPos)
      (workerId, ((xOffset, xSize), (yOffset, ySize)))
    }.toMap

    workerSpans.map {
      case (workerId, ((xOffset, xSize), (yOffset, ySize))) =>

        val localIds: Set[CellId] = (for {
          x <- xOffset until (xOffset + xSize)
          y <- yOffset until (yOffset + ySize)
        } yield gridMultiCellIdMap(GridCellId(x, y))).flatMap(ids => ids.toSet).toSet

        val remoteIds: Set[CellId] = localIds.flatMap(id => getNeighboursOf(id)).diff(localIds)

        (workerId, (localIds, remoteIds))
    }
  }

  private def getNeighboursOf(id: CellId): List[CellId] = {
    id match {
      case gridMultiCellId@GridMultiCellId(_, _, _) =>
        val cardinalNeighbours: List[GridMultiCellId] = multiConnectionNeighbours(gridMultiCellId)
          .cardinalNeighbourhood
          .values
          .map(boundary => boundary.boundaries)
          .flatMap(segmentCellId => segmentCellId.values)
          .toList
        val diagonalNeighbours: List[GridMultiCellId] = multiConnectionNeighbours(gridMultiCellId)
          .diagonalNeighbourhood
          .values
          .toList
        (cardinalNeighbours ++ diagonalNeighbours).filter(neighbour => neighbour != null)
      case _ => neighboursMutable(id).values.toList
    }
  }

  private def split(value: Int, parts: Int): Seq[Int] = {
    if (parts <= 0) {
      Seq.empty
    } else {
      val quotient: Int = value / parts
      val remainder: Int = value % parts

      Seq.tabulate(parts) {
        case index if index < remainder => quotient + 1
        case _ => quotient
      }
    }
  }
}
