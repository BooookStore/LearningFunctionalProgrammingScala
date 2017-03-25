/**
二分木を表すデータ構造と、コンパニオンオブジェクトの定義
*/

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A],right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

  val st = Branch(Branch(Leaf(10),Leaf(20)),Branch(Leaf(30),Leaf(40)))

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(map(l)(f),map(r)(f))
  }

  def fold[A,B](t: Tree[A])(lf: A => B)(bf: (B,B) => B): B = t match {
    case Leaf(v) => lf(v) //葉をB型へ変換
    case Branch(l,r) => bf(fold(l)(lf)(bf),fold(r)(lf)(bf))
  }

}
