/**
参照通過性がないコード
*/
object FailingFn {
  
  def failingFn1(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // 例外をスロー
    try {
      val x = 42 + 5
      x + y
    } catch {
      case e: Exception => 43
    }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // 例外をスロー
    } catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Double =
    if(xs.isEmpty)
      throw new ArithmeticException("mean of empty list!")
    else xs.sum / xs.length

}
