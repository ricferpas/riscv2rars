package util

object math {
  def pwl(v: Float) = if (v < -1.0f) -1.0f else if (v > 1.0f) 1.0f else v
}