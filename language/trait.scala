/**
traitについて
参考: http://docs.scala-lang.org/tutorials/scala-for-java-programmers.html
*/

/**
比較を行うtrait。
比較対象のインスタンスより自分のほうが小さいと判断されるメソッド(<)とequalsメソッドを、
継承クラスが実装すればその他の比較を行うメソッドを使用することができる。
*/
trait Ord {
  def < (that: Any): Boolean
  def <=(that: Any): Boolean = (this < that) || (this == that)
  def > (that: Any): Boolean = !(this <= that)
  def >=(that: Any): Boolean = !(this < that)
}

/**
年月日を表すクラス。
Ord traitを継承しているため、Dateインスタンス同士の比較が可能。
*/
class Date(y: Int, m: Int, d: Int) extends Ord {
  def year = y
  def month = m
  def day = d

  override def toString(): String = year + "-" + month + "-" + day

  override def equals(that: Any): Boolean =
    this.isInstanceOf[Date] && {
      val o = that.asInstanceOf[Date]
      o.day == day && o.month == month && o.year == year
    }

    def <(that: Any): Boolean = {
      if(!that.isInstanceOf[Date])
        throw new Exception("cannot compare" + that + " and a Date")

      val o = that.asInstanceOf[Date]
      (year < o.year) ||
      (year == o.year && (month < o.month ||
                         (month == o.month && day < o.day)))
    }
}
