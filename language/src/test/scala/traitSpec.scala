import org.scalatest._

class traitScala extends FlatSpec with Matchers {

  "Date" should "toStringの結果が、コンストラクタの内容と一致" in {
    val date = new Date(1999,1,1)
    assert(date.toString == "1999-1-1")
  }

  it should "equalsメソッドが正しく動作する" in {
    val date1 = new Date(1999,1,1)
    val date2 = new Date(1999,1,1)
    assert(date1.equals(date2))
  }

  it should "equalsメソッドにDate以外のオブジェクトが与えられた時falseを返す" in {
    val date = new Date(1999,1,1)
    a [java.lang.ClassCastException] should be thrownBy {
      assert(date.equals(100))
    }
  }

  it should "<メソッドが正しく動作する" in {
    val date1 = new Date(2000,1,1)
    val date2 = new Date(2001,1,1)
    assert(date1 < date2)
  }

}
