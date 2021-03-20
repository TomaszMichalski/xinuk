package pl.edu.agh.continuous.env.model.continuous

case class SignalVector(x: Double, y: Double) {
  def +(other: SignalVector): SignalVector = SignalVector(x + other.x, y + other.y)

  def normalize: SignalVector = SignalVector(x / this.length, y / this.length)

  def length: Double = math.sqrt(x * x + y * y)
}

object SignalVector {
  private final val Zero = SignalVector(0d, 0d)

  def zero: SignalVector = Zero
}
