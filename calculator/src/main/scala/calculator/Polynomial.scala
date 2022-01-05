package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal {
    math.pow(b(), 2) - 4 * a() * c()
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    val deltaValue = delta()
    deltaValue match {
      case d if d > 0 => Set((-b() + math.sqrt(d))/(2*a()), (-b() - math.sqrt(d))/(2*a()))
      case 0 => Set(-b() / (2 * a()))
      case _ => Set[Double]()
    }
  }
