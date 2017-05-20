import org.scalatest._

/**
* カリー化に関するコード
*/
class Curry extends FlatSpec with Matchers {

  "Curry" should "カリー化してみる" in {

    val sum = (a: Int) => (b: Int) => a + b

    val sum1 = sum(1)
    val result = sum1(2)

    sum(1)(2) should be (3)
    result should be (3)
  }

  it should "リストを使用したカリー化" in {
    val each = (a: List[Int]) => (f: (Int,Int) => Int) =>
      a.reduce(f)

    val numberList = each(List(1,2,3,4))
    numberList((a,b) => a + b) should be (10)
    numberList((a,b) => a * b) should be (24)
  }

}
