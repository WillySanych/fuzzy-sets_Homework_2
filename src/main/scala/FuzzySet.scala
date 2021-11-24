object FuzzySet {
  class Universe[T](val values: Set[T])
}

class FuzzySet[T](val m: T => Double) {
  import FuzzySet.Universe

  def isEmpty(implicit universe: Universe[T]): Boolean =
    universe.values.forall(m(_) == 0.0)

  def equalTo(that: FuzzySet[T])(implicit universe: Universe[T]): Boolean =
    universe.values.forall(i => m(i) == that.m(i))

  def contains(value: T): Double =
    m(value)

  def union(that: FuzzySet[T]): FuzzySet[T] =
    new FuzzySet[T](i => m(i).max(that.m(i)))

  def intersect(that: FuzzySet[T]): FuzzySet[T] =
    new FuzzySet[T](i => m(i).min(that.m(i)))

  def complement(implicit universe: Universe[T]): FuzzySet[T] =
    new FuzzySet[T](i => if (universe.values.contains(i)) (1 - m(i)) else 0.0)

}

object FuzzySetApp extends App {
  import FuzzySet.Universe

  implicit val fuzzySetUniverse: Universe[Int] = new Universe(Set.from(1 to 10))

  val emptyFuzzySet = new FuzzySet[Int](_ => 0.0)

  val someNonEmptyFuzzySet_1 = new FuzzySet[Int]({
    case 1 => 0.5
    case 2 => 0.75
    case 3 => 1
    case _ => 0.0
  })

  val someNonEmptyFuzzySet_2 = new FuzzySet[Int]({
    case 1 => 0.7
    case 2 => 0.8
    case 3 => 0.5
    case _ => 0.0
  })

  val someNonEmptyFuzzySet_3 = new FuzzySet[Int]({
    case 1 => 0.7
    case 2 => 0.8
    case 3 => 0.5
    case _ => 0.0
  })

  println(emptyFuzzySet.isEmpty)
  println(someNonEmptyFuzzySet_1.isEmpty)
  println(someNonEmptyFuzzySet_1.contains(1))
  println(someNonEmptyFuzzySet_1.equalTo(someNonEmptyFuzzySet_2))
  println(someNonEmptyFuzzySet_2.equalTo(someNonEmptyFuzzySet_3))
  println((someNonEmptyFuzzySet_1.intersect(someNonEmptyFuzzySet_2)).contains(3))
  println((someNonEmptyFuzzySet_1.complement).contains(1))
}