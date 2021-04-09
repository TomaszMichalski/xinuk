package pl.edu.agh.continuous.env.model.continuous

case class Vector(x: Double, y: Double) {
  def lengthSquared: Double = x * x + y * y
}

object Vector {
  private final val Zero = Vector(0d, 0d)

  def zero: Vector = Zero
}
