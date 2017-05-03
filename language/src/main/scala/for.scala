/**
 * for(...) yield ... について。
 */

/**
 * for(...) yield ... 文を利用したサンプルコード
 */
object SampleForYield {

  def print1to10(): Unit =
    for (i <- 1 to 10) println(i)

  def print1unit10(): Unit =
    for (i <- 1 until 10) println(i)

  def listIteration(): Unit = {
    val strs = "A" :: "B" :: "C" :: Nil
    for (str <- strs) println(strs)
  }

  def generate1to5Numbers(): IndexedSeq[Int] =
    for (i <- 1 to 5) yield i

  def generate1to5NumbersStrings(): IndexedSeq[String] =
    for (i <- 1 to 5) yield i.toString

  def generateNumbersToupleString(): IndexedSeq[(String, String)] =
    for {
      i <- 1 to 5
      j <- 5 to 10
    } yield (i.toString, j.toString)
}

/**
 * for (...) yield ... 文の簡易的な実装をしたコード
 */
object ImplementationOfForYield {

  def print1to10(): Unit = {
    (1 to 10).foreach {
      (i) => println(i)
    }
  }

  def generate1to5NumbersStrings(): IndexedSeq[String] = {
    (1 to 5).map {
      (i) => i.toString
    }
  }

  def generateNumbersToupleString(): IndexedSeq[(String, String)] = {
    (1 to 5).flatMap {
      (i) =>
        (5 to 10).map {
          (j) => (i.toString, j.toString)
        }
    }
  }
}

/**
 * for (...) yield ... 文で使用可能なクラス。
 */
class Sample(a: Int, b: Int, c: Int) {

  def foreach[A](f: Int => A): Unit = {
    f(a)
    f(b)
    f(c)
  }

  def map(f: Int => Int): Sample = {
    new Sample(f(a), f(b), f(c))
  }

}
