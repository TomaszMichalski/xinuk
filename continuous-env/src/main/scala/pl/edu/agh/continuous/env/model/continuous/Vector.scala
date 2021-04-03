package pl.edu.agh.continuous.env.model.continuous

case class Vector(x: Double, y: Double)

object Vector {
  private final val Zero = Vector(0d, 0d)

  def zero: Vector = Zero
}
