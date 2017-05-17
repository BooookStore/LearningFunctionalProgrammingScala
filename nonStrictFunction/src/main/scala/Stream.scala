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
  * 無限ストリームを作成する。zは初期状態を表し、述語fによって次の値が決定される。
  */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h,s)) => cons(h, unfold(s)(f))
    case None => empty
  }


}
