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
}
