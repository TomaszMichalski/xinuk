package pl.edu.agh.continuous.env.model.continuous

case class Obstacle(xs: Array[Int], ys: Array[Int], points: Int)

case class ObstacleSegment(a: (Int, Int), b: (Int, Int))
