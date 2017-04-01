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
    case Left(_) => Left(_)
    case Right(a) => f(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    for {
      aa <- this
      bb <- b
    } yield f(aa,bb)
  }



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

}
