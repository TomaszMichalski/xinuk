package pl.edu.agh.continuous.env.model.continuous

object MovementDecision extends Enumeration {
  type MovementDecision = Value

  val Free, SlideForward, SlideBackward, Stuck = Value
}
