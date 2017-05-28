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
   * 
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
