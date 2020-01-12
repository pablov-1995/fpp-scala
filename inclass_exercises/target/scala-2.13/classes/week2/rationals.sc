class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")
  @scala.annotation.tailrec
  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)
  def numer = x/gcd(x, y)
  def denom = y/gcd(x, y)
  def + (s: Rational) =
    new Rational(numer * s.denom + s.numer * denom, denom*s.denom)
  def - (s: Rational) =
    new Rational(numer * s.denom - s.numer * denom, denom*s.denom)
  def unary_- = new Rational(-numer, denom)
  def < (s: Rational) = numer * s.denom < s.numer * denom
  def max(s: Rational) = if (this < s) s else this
  override def toString =
    numer + "/" + denom
}

val x = new Rational(1, 2)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
val w = new Rational(3, 9)
x.numer
x.denom
-x
x < y
x max y
