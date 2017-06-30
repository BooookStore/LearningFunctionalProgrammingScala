
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

  // 並列処理を行った結果のデータを格納する型が必要だと推測できる。
  // なぜなら、並列処理をした結果のデータ型は様々であるためである。

  /*
   * Parという型を想定する。これは、Parallelの略称である。
   * 以下の関数を持たせる。
   *
   * 1. def unit[A](a: => A): Par[A]
   *    未評価のAを受け取り、クライアント側と異なるスレッドで値を評価する。
   *
   * 2. def get[A](a: Par[A]): A
   *    Parが保持しているAを取得する。
   *
   * 3. def map2[A, B, C](a: Par[A], b: Par[B])((A,B) => C): Par[C]
   *    マップ関数
   *
   */

  // 実装その３
  // 並列実行で計算を行う。
  // 上記で想定しているParを使用すれば、以下のように並列処理を行うことが可能になる。
  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if(ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length/2)
      Par.map2(sum(l), sum(r))(_ + _)
    }
}
