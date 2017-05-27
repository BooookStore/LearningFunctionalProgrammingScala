/*
 * 遅延評価を行う機能を提供する。
 */

import Stream._ // 自分自身をインポートすることで、定義の順序に依存せずに、各オブジェクト、関数等が利用可能

/**
 * 遅延評価に関する操作を表すトレイト。ケースクラスとしてConsクラスが実装を持っている。
 * 更に詳しい内容についてはコンパニオンオブジェクトを参照。
 */
sealed trait Stream[+A] {

  /**
   * ストリームの先頭の値を返す。先頭の値がある時のみ、先頭の値が評価される。
   */
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /**
   * ストリームをリストに変換し、返す。
   */
  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  /**
   * 先頭からn個をStreamとして返す。
   */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  /**
   * 先頭から順にpを適用し、Falseとなった時、それまでの A を Stream として返す。
   */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if (p(h())) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  /**
   * 関数 f の第２引数は名前優先（必要とされた場合評価される）のため、第一引数で戻り値が決定した場合、
   * Streamの全ての値は評価されない。
   */
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exist(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  /**
   * takeWhile を foldRight を使用して実装。
   */
  def takeWhileByFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def map[B](f: A => B): Stream[B] =
    foldRight(empty: Stream[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty: Stream[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](m: => Stream[B]): Stream[B] =
    foldRight(m)((a, b) => cons(a, b))

  def flatMap[B >: A](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  def mapByUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }

  def takeByUnfold(i: Int): Stream[A] =
    unfold((this, i)) {
      case (Cons(h, t), 1) => Some(h(), (empty, 0))
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileByUnfold(f: A => Boolean): Stream[A] =
    unfold((this)) {
      case Cons(h, t) if f(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()) /* ヘッド値 */ , (t1(), t2()) /* 継続のストリーム */ ))
      case _ => None
    }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A], t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1(), t2()))
    }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /**
   * 与えられたストリームの要素で、自身のストリームが始まっているかを返す。
   *
   * 例：Stream(1,2,3,4).startsWith(Stream(1,2,3)) == true
   *    Stream(1,2,3,4).startsWith(Stream(2,3,4)) == false
   *
   * 動作手順
   * 1. 与えられたストリームをzipAllで自身の要素とタプル値にする。
   *     [1,2,3] zipAll [1,2] -> [(1,1),(2,2),(3,empty)]
   * 2.takeWhileでタプル値の２番目の要素までのストリームを得る。
   *     [(1,1),(2,2),(3,empty)] takeWhile(...) -> [(1,1),(2,2)]
   * 3.forAllで、タプル値の要素が同値かどうかを返す。
   *     [(1,1),(2,2)] forAll {...} -> true
   */
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h, h2) => h == h2
    }

  /**
   * トラバース可能なコレクションの末端を反復処理します。最初の値は自身と同じトラバース可能なストリームであり、
   * 最後の値は空のストリームです。その間の処理は補完された値となります。
   *
   * 例：Stream(1,2,3).tails -> Stream(Stream(1,2,3),Stream(2,3),Stream(3),Stream())
   */
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append Stream(empty)

  /**
   * 与えられたStreamを一部または全体として保持しているかどうかを返します。
   * 例: Stream(1,2,3).hasSubsequence(Stream(1,2,3)) = true
   *    Stream(1,2,3,4).hasSubsequence(Stream(2,3)) = true
   *    Stream(1,2,3).hasSubsequence(Stream(3,4,5)) = false
   *
   * 動作手順：
   * 1.tailsで、自身のストリームから比較用のストリームを作成
   *     Stream(1,2,3).tails -> Stream(Stream(1,2,3),Stream(2,3),Stream(3),Stream())
   * 2.existsで、比較用ストリームの中に、対象のストリームが存在するかを確認
   *  2.1 existの引数に _ startsWith s2 とすることで、比較用ストリームの中でs2のストリームで始まるものがあるかを探索
   *
   * [MEMO]
   * この動作は難しく見えるが、単にtailsによって、ストリームの初めの要素を無くしていって、
   * それぞれのストリームの中に、与えられたストリームで始まるものがあるかを評価している。
   */
  def hasSubsequence[B >: A](s2: Stream[B]): Boolean =
    tails exists (_ startsWith s2)

  def scanRight[B](rv: B)(f: (A, => B) => B): Stream[B] =
    this.tails.map((x) => x.foldRight(rv)(f))
}

/**
 * Streamが空であることを表現する。
 */
case object Empty extends Stream[Nothing]

/**
 * Streamを実装した、遅延評価を行うケースクラス。２つの引数の評価は必ず１回のみとなる。
 * そのため、評価に時間がかかるものを処理するのに適している。
 */
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

/**
 * Streamトレイとのコンパニオンオブジェクト。
 */
object Stream {

  /**
   * Consクラスを新しく生成する。２つの引数は一度評価され、その後Consクラスにセットされる。
   *
   * [MEMO] 未評価の引数は thunk と呼ばれる。
   */
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /**
   * aの値をもつ無限ストリームを返す
   * 例： constant(1) == [1,1,1,1,...]
   */
  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  /**
   * nから始まり、１づつ増加する値を表す無限ストリームを返す
   * 例： from(3) -> [3,4,5,6,7,...]
   */
  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  /**
   * フィボナッチ数列を表す無限ストリームを返す
   */
  def fibs = {
    def go(f0: Int, f1: Int): Stream[Int] =
      cons(f0, go(f1, f0 + f1))
    go(0, 1)
  }

  /**
   * 無限ストリームを作成する。zはストリームの先頭要素を表し、述語fによって次の値が決定される。
   * 熟語fが返す値Option[(A, S)]の、Aはストリームの値である。
   * 一方SはAの次の要素を決定する為に述語fの引数として使用される。
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None => empty
  }

  /**
   * フィボナッチ数列を表すストリームを返す。unfoldを使用して実装。
   */
  def fibsByUnfold = unfold((0, 1))((p) => {
    val value = p._1 + p._2
    val next = (p._2, value)
    Some(value, next)
  })

  /**
   * fromをunfoldを使用して実装。
   */
  def fromByUnfold(n: Int): Stream[Int] = unfold(n)((p) => Some(p, p + 1))

  /**
   * constantをunfoldを使用して実装。
   */
  def constantByUnfold(n: Int): Stream[Int] = unfold(n)((p) => Some(p, p))

}
