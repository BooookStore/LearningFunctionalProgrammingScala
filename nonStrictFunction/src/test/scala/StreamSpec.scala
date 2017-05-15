import org.scalatest._

class StreamSpec extends FlatSpec with Matchers {

  "Stream.forAll method" should "保持する値に対して、引数のメソッドを適用し、結果をbooleanで返す" in {
    Stream(1,1,1,1).forAll(_ == 1) should be (true)
    Stream("A","B","C").forAll(_.length == 1) should be (true)
    Stream("AA","BB","CC").forAll(_.length == 1) should be (false)
  }

  "Stream.takeWhileByFoldRight method" should "保持する値に対して、引数のメソッドを順に適用し、結果がfalseになるまでの値をStreamで返す" in {
    Stream(1,2,3,4).takeWhileByFoldRight(_ <= 3).toList should be (List(1,2,3))
    Stream(1,2,3,4).takeWhileByFoldRight(_ <= 4).toList should be (List(1,2,3,4))
  }

}
