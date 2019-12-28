package se.kth.edx.id2203.tutorial

package object gameoflife {
  def stateWrap(size: Int): Int => Int = { x =>
    if (x < 0) {
      size + (x % size)
    } else {
      x % size
    }
  }
}
