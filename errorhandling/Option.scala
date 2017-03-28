sealed trait Option[+A] {

  /**
  Optionの持つ値を別の値へと変更します。
  OptionがNoneの場合、変更は実施されずNoneが返されます。
  */
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  /**
  Optionの持つ値を別の値へと変更します。
  OptionがNoneの場合、変更は実施されずNoneが返されます。

  mapと異なる点は、持っている値から新しいOptionを生成する点です。
  よって、持っている値の変更結果がOptionになる関数を引数として与える場合、Optionの持つ値が
  Optionになってしまうという２重構造のOptionの生成を防ぐことが可能です。
  */
  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x -m,2))))

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  // ２つのOptionが持つ値より、計算を行い、計算結果をOptionとして返します。
  def map2[A,B,C](a: Option[A], b:Option[B])(f: (A,B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa,bb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  // 特定の型から特定の型への変換を試みる
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def parseInts(a: List[String]): Option[List[Int]] =
    sequence(a map (i => Try(i.toInt)))

  /**
  リストが持つ値を特定の値へと変更する。
  値の変更において、例外が発生した場合戻り値はNoneとなる。
  */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => f(h) flatMap (hh => traverse(t)(f) map (hh :: _))
  }

}
