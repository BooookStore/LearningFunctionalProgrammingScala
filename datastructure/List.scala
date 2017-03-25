/**
単方向リストを表すデータ構造と、コンパニオンオブジェクトの定義
*/

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def tail[A](cons: List[A]): List[A] = cons match {
    case Nil => Nil
    case Cons(x,xs) => xs
  }

  def setHead[A](head: A, list: List[A]): List[A] =
    Cons(head, list)

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else drop(tail(l), n - 1)

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(h,t) => if (f(h)) l else dropWhile(tail(l))(f)
      case Nil => Nil
    }

  /**
  リストasに対して再帰的にfを行い、結果を返す。

  @param as 操作対象となるリスト
  @param z 最後尾のNilデータに対応する値
  @param f 左と右にあるデータに対して行う操作
  */
  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  /**
  リストasに対して再帰的にfを行い、結果を返す。

  @param as 操作対象となるリスト
  @param z 関数fの第一引数に当たる初期値
  @param f 第一引数に今までの計算結果を、第２引数に新たに取り出した値が入力される。計算結果は次の再帰呼び出しの中でfの第一引数になるか、リストの末端に到達したときに再帰呼び出し全体の戻り値になる。

  List[Int]の全ての値を合計する例
  @example foldLeft(Cons(1, Cons(2, Cons(3 ,Nil))), 0)(_ + _) == 6
  */
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B =
    as match {
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z,h))(f)
    }

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((x,y) => Cons(y,x))

  def append[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1,l2)(Cons(_,_))
  }

  def concat[A](as: List[List[A]]): List[A] = {
    foldRight(as, Nil: List[A])(append)
  }

  val sampleIntList = List(1,2,3)
  val sampleDoubleList = List(1.0,2.0,3.0)

  /**
  数値リストの各値に対して、１を加算したリストを返す。
  */
  def add_1(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h,z) => Cons(h + 1,z))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h,z) => Cons(h.toString,z))

  /**
  リスト内の値を特定の値へ変換する。
  @param l 変換対象のリスト。
  @param f 変換方法。引数はlが保持する値の一つ。

  @example map(List(1,2,3))(_ + 1) // 中身に対して１を加える。
  */
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h,z) => Cons(f(h),z))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A])((h,z) => if(f(h)) Cons(h,z) else z)

  /**
  リスト内が持つ個々の値を任意に変換、増加させる。
  @param l 変換対象のリスト
  @param f 変換方法。第一引数がlが持つ値の一つ、戻り値がリストとなる。

  @example flatMap(List(1,2,3))(i => List(i,i+1)) == List(1,2,2,3,3,4)
  */
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(i => if (f(i)) List(i) else Nil)

  def addPairwise(a: List[Int], b:List[Int]): List[Int] =
    (a,b) match {
      case (Nil,_) => Nil
      case (_,Nil) => Nil
      case (Cons(h1,t1),Cons(h2,t2)) => Cons(h1+h2,addPairwise(t1,t2))
    }

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] =
    (a,b) match {
      case (Nil,_) => Nil
      case (_,Nil) => Nil
      case (Cons(h1,t1),Cons(h2,t2)) => Cons(f(h1,h2),zipWith(t1,t2)(f))
    }

}
