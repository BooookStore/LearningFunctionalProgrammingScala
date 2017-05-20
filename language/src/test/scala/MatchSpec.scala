import org.scalatest._

class Match extends FlatSpec with Matchers {

  "Match" should "matchの省略" in {

    // 関数リテラルにて、match文で戻り値を決定する時、matchを省略できる。
    val f: (Int) => Boolean = {
      case 10 => true
      case _ => false
    }

    f(10) should be (true)
  }

  it should "matchの省略２" in {
    def isThis(l: Int, f: Int => Boolean): Boolean = f(l)
    val result =
      // 引数となる関数が、match文で戻り値を決定する時、matchを省略できる。
      isThis(10, {
        case 10 => true
        case _ => false
      })

    result should be(true)

  }

}
