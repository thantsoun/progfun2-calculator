package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    val solution: (Double, Double, Double, Double) => Double = (a, b, d, m) => (-b + (m * math.sqrt(d))) / (2 * a)
    Signal({
      if (delta() < 0) Set()
      else if (delta() == 0) Set(solution(a(), b(), delta(), 0))
      else Set(solution(a(), b(), delta(), 1), solution(a(), b(), delta(), -1))
    })
  }
}
