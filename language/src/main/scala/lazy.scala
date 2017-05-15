object Lazy extends App {
  val x = mayBwTwice(true,  { println("Hello"); 20+2; })
  println(x)

  val y = mayBwTwice2(true, { println("Hello"); 20+2; })
  println(y)

  def mayBwTwice(b: Boolean, i: => Int) = if (b) i+i else 0

  def mayBwTwice2(b: Boolean, i: => Int) = {
    lazy val j = i
    if (b) j+j else 0
  }
}
