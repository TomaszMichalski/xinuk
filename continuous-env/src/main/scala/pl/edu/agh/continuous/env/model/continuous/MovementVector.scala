package pl.edu.agh.continuous.env.model.continuous

case class MovementVector(a: (Double, Double), b: (Double, Double)) {
  def x: Double = b._1 - a._1

  def y: Double = b._2 - a._2

  def lengthSquared: Double = x * x + y * y

  def length: Double = math.sqrt(lengthSquared)
}
