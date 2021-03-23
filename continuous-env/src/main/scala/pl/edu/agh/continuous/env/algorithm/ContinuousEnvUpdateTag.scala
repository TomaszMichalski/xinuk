package pl.edu.agh.continuous.env.algorithm

import pl.edu.agh.xinuk.algorithm.UpdateTag

object ContinuousEnvUpdateTag {

  case object Leave extends UpdateTag

  case object Arrive extends UpdateTag

  case object Stay extends UpdateTag
}
