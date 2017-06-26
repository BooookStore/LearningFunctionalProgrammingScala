
//　リストの整数値を合計する方法を列挙してみる。
object SeqSum {

  // 実装その１
  def sum_first(ints: Seq[Int]): Int =
    ints.foldLeft(0)((a,b) => a + b)

  // 実装その２
  // IndexedSeqはスタンダードライブラリの一つのクラス。
  // Vectorのような、ランダムアクセスリストのスーパークラスである。
  // リストとは異なり、効率的な２分割関数(splitAt)を提供してくれる。
  def sum_second(ints: IndexedSeq[Int]): Int =
    if(ints.size <= 1)
      ints.headOption getOrElse 0 // headOptionはcollectionすべてが実装している。
    else {
      val (l,r) = ints.splitAt(ints.length/2) // リストを２つに分割する。
      sum_second(l) + sum_second(r) // ２分割したリストから再帰的に関数を呼び出し、計算結果を返す。
    }

}
