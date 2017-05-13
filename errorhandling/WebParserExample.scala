// Webのフォーム入力値を、StringからIntへ変換する際にOptionを利用する例です。
class WebParserExample {

  // 保険料金の見積もりを、フォームの入力値から作成
  def parseInsuranceRateQuote(
    age: String,
    numberOfSpeedingTickets: String): Option[Double] = {
      val optAge: Option[Int] = Try { age.toInt }
      val optTickets: Option[Int] = Try { numberOfSpeedingTickets.toInt }

      map2(optAge,optTickets)(insuranceRateQuote)
    }

  // 保険料金の見積もりを計算
  def insuranceRateQuote(age: Int, tickets: Int): Double = {
    tickets / age
  }

  // 特定の型から特定の型への変換を試みる
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  // insuranceRateQuote関数の引数のシグネチャをOptionにしなくて済むようにする関数
  // この関数を汎用的なものにすれば、Optionの値を取り出し処理を行う作業を、
  // 既存の関数の引数のシグネチャをOptionに書き直さなくて済む。
  def map2[A,B,C](a: Option[A], b:Option[B])(f: (A,B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa,bb)))
}
