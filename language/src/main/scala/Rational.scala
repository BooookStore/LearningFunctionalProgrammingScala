
/**
 * 分数を表現するプリミティブクラス
 *
 * @param n 分子
 * @param d 分母
 */
class Rational(n: Int, d:Int) {

  /** 分母が０出ない場合において、オブジェクトを生成 */
  require(d != 0)

  /** 分子 */
  val number: Int = n

  /** 分母 */
  val denom: Int = d

  /** 現在持っている分子、分母の値を理解しやすい形で出力する */
  override def toString = number + "/" + denom

  /** 分数同士の足し算を行い、結果を返す */
  def add(that: Rational): Rational =
    new Rational(
      number * that.denom + that.number * denom,
      denom * that.denom
    )
}
