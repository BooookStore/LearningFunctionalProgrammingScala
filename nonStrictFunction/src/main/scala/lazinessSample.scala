
object App extends App {

  val a = 20

  Sample.if2(a < 22,
    () => println("A"),
    () => println("B"))

  val r = Sample.if2(false, sys.error("fail"), 3)

  Sample.mayBeTwice(true, {println("Hi"); 1+41})

}

object Sample {

  /**
   * condによって、onTrue、onFalseを評価し、返す。
   * 引数で渡された時点では、引数が評価されない。
   */
  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse

  def mayBeTwice(b: Boolean, i: => Int) = {
    lazy val j = i
    if (b) j+j else 0
  }

}
