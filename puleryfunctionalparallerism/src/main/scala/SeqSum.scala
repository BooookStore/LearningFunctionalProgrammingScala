
//　リストの整数値を合計する方法を列挙してみる。
object SeqSum {

  // 実装その１
  def sum_first(ints: Seq[Int]): Int =
    ints.foldLeft(0)((a,b) => a + b)

  // 実装その２
  def sum_second(ints: IndexedSeq[Int]): Int =
    if(ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      sum(l) + sum(r)
    }

}
