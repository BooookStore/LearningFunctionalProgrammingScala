import org.scalatest._

class StreamSpec extends FlatSpec with Matchers {

  "Stream.forAll method" should "保持する値に対して、引数のメソッドを適用し、結果をbooleanで返す" in {
    Stream(1, 1, 1, 1).forAll(_ == 1) should be(true)
    Stream("A", "B", "C").forAll(_.length == 1) should be(true)
    Stream("AA", "BB", "CC").forAll(_.length == 1) should be(false)
  }

  "Stream.takeWhileByFoldRight method" should "保持する値に対して、引数のメソッドを順に適用し、結果がfalseになるまでの値をStreamで返す" in {
    Stream(1, 2, 3, 4).takeWhileByFoldRight(_ <= 3).toList should be(List(1, 2, 3))
    Stream(1, 2, 3, 4).takeWhileByFoldRight(_ <= 4).toList should be(List(1, 2, 3, 4))
  }

  "Stream.map method" should "全ての値を変換する" in {
    Stream(1, 2, 3).map(_.toString).toList should be(Stream("1", "2", "3").toList)
  }

  "Stream.filter method" should "全ての値に対してフィルタリングを行う" in {
    Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).filter(_ % 2 == 0).toList should be(List(2, 4, 6, 8, 10))
  }

  "Stream.append method" should "ストリームの最後に要素を追加する" in {
    Stream(1, 2, 3).append(Stream(4, 5, 6)).toList should be(List(1, 2, 3, 4, 5, 6))
  }

  "Stream.flatMap method" should "与えられた関数によって、ストリーム全体をマップ" in {
    Stream(1, 2, 3).flatMap((a) => Stream(a, a)).toList should be(List(1, 1, 2, 2, 3, 3))
  }

  "Stream.constant" should "与えられた値で無限配列を作成" in {
    Stream.constant(1).take(5).toList should be(List(1, 1, 1, 1, 1).toList)
  }

  "Stream.from" should "与えられた値から１ずつ増加する無限配列を作成" in {
    Stream.from(5).take(5).toList should be(List(5, 6, 7, 8, 9).toList)
  }

  "Stream.zipWith" should "二つのストリームを一つのストリームへ変換" in {
    Stream(1, 2, 3).zipWith(Stream(1, 2, 3))((a, b) => a + b).toList should be(List(2, 4, 6))
  }

  "Stream.zipAll" should "二つのストリームを一つのストリームへ変換" in {
    Stream(1, 2, 3).zipAll(Stream(1, 2, 3)).toList should be(List(Some(1) -> Some(1), Some(2) -> Some(2), Some(3) -> Some(3)))
  }
}
