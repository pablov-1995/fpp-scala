type FunSet = Int => Boolean
def contains(s: FunSet, elem: Int): Boolean = s(elem)
contains(x => x>10, 7)
def singletonSet(elem: Int): FunSet = (x => x == elem)
singletonSet(5)
def union(s: FunSet, t: FunSet): FunSet = (x => (s(x) || t(x)))
def intersection(s: FunSet, t: FunSet): FunSet = (x => (s(x) & t(x)))
def diff(s: FunSet, t: FunSet): FunSet = (x => contains(union(s, t), x) && !contains(intersection(s, t), x))
def filter(s: FunSet, p: Int => Boolean): FunSet = (x => (s(x) & p(x)))

union(x => x > 100, x => x <= 200)