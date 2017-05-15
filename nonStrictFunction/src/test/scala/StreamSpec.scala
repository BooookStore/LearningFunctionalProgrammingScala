import org.scalatest._

class StreamSpec extends FlatSpec with Matchers {

  "Stream.forAll method" should "保持する値に対して、引数のメソッドを適用し、結果をbooleanで返す" in {
    Stream(1,1,1,1).forAll(_ == 1) should be (true)
    Stream("A","B","C").forAll(_.length == 1) should be (true)
    Stream("AA","BB","CC").forAll(_.length == 1) should be (false)
  }

}
