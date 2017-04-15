sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E,B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case Right(a) => Right(a)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(a) => Left(a)
    case Right(a) => f(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa,bb)

}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  /**
  Eitherの使用例
  */
  def mean(xs: List[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list.")
    else
      Right(xs.sum / xs.length)

  /*
  リストに格納された値を特定の値へと変更する。
  リストに格納されているすべての値の変換が成功すれば、変換後の値をリストとして保持するRight型を
  返す。
  変換に失敗した際にはエラーメッセージを持つLeft型を返す。
  */
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)

      // 現在のリストのヘッド値を関数fによって変換
      // 後続のリストの値を再帰呼び出しにより変換
      // これら２つの値をmap2関数によってリスト化
      case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
    }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

}
