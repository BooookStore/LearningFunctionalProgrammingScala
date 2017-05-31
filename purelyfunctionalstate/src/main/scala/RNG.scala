/**
 * Random Number Generator
 * 乱数生成のインターフェースを定義
 */
trait RNG {
  def nextInt: (Int, RNG)
}

/**
 * 乱数を生成の機能を提供
 */
case class SimpleRNG(seed: Long) extends RNG {

  /**
   * NOTE
   * 状態を明示的に更新することで参照透明性を保持している。状態(seed)は内部で
   * 副作用として更新されず、単純に新しい"状態"として、生成した乱数とともに返される。
   *
   * 新しい状態をプログラム全体に伝えることから、次の状態が何であるかを計算する
   * 関心事を分離している。なので、クライアントコードは返されたRNGに対してnextInt
   * を呼び出すだけで良い。
   */
  def nextInt: (Int, RNG) = {

    // ビット演算で新しい seed を生成
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    // 右シフト演算によって乱数を生成
    val n = (newSeed >>> 16).toInt

    (n, nextRNG)
  }
}

/**
 * 乱数生成に関するユーティリティ
 */
object RandomUtil {

  /**
   * 乱数を２つ生成する。
   */
  def randomPair(rng: RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  /**
   * 整数の乱数を生成する。
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  /**
   * 0以上１未満の乱数をDouble値で生成する。
   */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = rng.nextInt
    val i2 = i / Int.MaxValue.toDouble + 1
    (i2, r)
  }

  /**
   * Int型の乱数と、0以上１未満のDouble型の乱数を返す。
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = RandomUtil.double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = RandomUtil.double(r1)
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = RandomUtil.double(rng)
    val (d2, r2) = RandomUtil.double(r1)
    val (d3, r3) = RandomUtil.double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (List[Int](), rng)
    case _ => {
      val (l, r1) = ints(count - 1)(rng)
      val (i, r2) = r1.nextInt
      (i :: l, r2)
    }
  }

  // 関数を型として定義
  type Rand[+A] = RNG => (A, RNG)

  /**
   * RNGから、乱数と、RNGを得る関数オブジェクト
   */
  val int: Rand[Int] = _.nextInt

  /**
   * a を初期のペアとする Rand[A] を生成する
   */
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  /**
   * Randのマップ関数。 s はマップ元の関数であり、 f によってマップ処理が行われる。
   */
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

　/**
   * 正の数で、偶数の乱数を生成する関数を、生成
   *
   * 使用例：
   * val (i, r) = nonNegativeEvenGenerator(SimpleRNG(1))
   *
   *   i: Int = 384748
   *   r: RNG = SimpleRNG(25214903928)
   */
  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)
}
