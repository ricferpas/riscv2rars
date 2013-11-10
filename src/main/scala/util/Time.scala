package util
import scala.compat.Platform

object time {
  private def now = Platform.currentTime

  def executionTime[A](f: ⇒ A): Long = {
    val start = now
    f
    now - start
  }

  def during[A](t: Long)(f: ⇒ A) {
    val end = now + t
    while (now < end) f
  }
}