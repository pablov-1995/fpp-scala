import week3._

new Rational(3,5)

@scala.annotation.tailrec
def nth[T](n: Int, xs: List[T]): T =
  if (n==0) xs.head
  else nth(n-1, xs.tail)

// val list = new Cons(1, new Cons(2, new Nil))
