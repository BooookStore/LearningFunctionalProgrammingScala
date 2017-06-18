import rng._
import rng.RandomUtil._

object RNGScenario extends App {

  {
    val intPair = RandomUtil.both(_.nextInt, _.nextInt)
    val ((r1, r2), nextRNG1) = intPair(SimpleRNG(2))

    println(s"both create random number ($r1) , ($r2)")
  }

  {
    val (r, n) = RandomUtil.nonNegativeLessThan(250)(SimpleRNG(3))
    println(s"非負数の、250以下の乱数 ($r)")
  }

  {
    // val (r, n) = RandomUtil.flatMap(rng => rng.nextInt)(a => RandomUtil.unit(a))(SimpleRNG(3))
    val (r, n) = RandomUtil.flatMap(_.nextInt)(RandomUtil.unit(_))(SimpleRNG(3))
    println(s"($r)")

    val (r1, n2) = RandomUtil.flatMap(RandomUtil.nonNegativeEven)(RandomUtil.unit(_))(SimpleRNG(5))
    println(s"($r1)")
  }
  {
    // nonNegativeIntを使用してサイコロを作成してみる。
    def rollDie: Rand[Int] = map(nonNegativeLessThan(6)) (_ + 1)
    val ri = rollDie(SimpleRNG(123))._1
    println(s"roll die ($ri)")
  }
  {
    // sequenceを使ってみる。
    val rands = nonNegativeInt _ :: nonNegativeInt _ :: nonNegativeInt _ :: Nil
    val rands_conv = sequence(rands)

    val (rl, ne) = rands_conv(SimpleRNG(123))
    rl.foreach(i => print(i + "."))
  }
  {
    // Stateケースクラスを使ってみる。

    // flatMapメソッドをコール
    val s1 = State(nonNegativeLessThan(10))
    s1.flatMap(i => {})
  }
}
