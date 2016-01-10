def sum(f: Int => Int, a: Int, b: Int): Int = {
  if (a > b) 0
  else f(a) + sum(f, a+1, b)
}

def sumSquared(a: Int, b: Int) = sum(x => x * x, a, b)

sumSquared(1,3)
sum(x => x * x, 1, 3)

object exercise {
  def sum(f: Int => Int, a: Int, b: Int) = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }
}

exercise.sum(x => x * x, 1, 3)


product(x => x)(1, 3)

def fact(n: Int) = product(x => x)(1, n)

fact(3)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a+1, b))
}

def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

product(x => x)(1, 3)


val x = new Rational(1,3)
val y = new Rational(5,7)
val z = new Rational(3,2)
x.denom
x.numer
x.add(new Rational(2,3))
x.neg
x.sub(y).sub(z)
y.add(y)
x.less(y)
x.max(y)
new Rational(2)


class Rational(x: Int, y: Int) {
  require(y != 0, "Denom must be nonzero")

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a
    else gcd(b, a % b)

  def numer = x / gcd(x, y)
  def denom = y / gcd(x, y)

  def add(that: Rational) =
    new Rational(
    this.numer * that.denom + that.numer * this.denom,
    this.denom * that.denom)

  def this(x: Int) = this(x, 1)

  def neg: Rational = new Rational(- numer, denom)

  def sub(that: Rational) = add(that.neg)

  def less(that: Rational) = this.numer * that.denom < that.numer * this.denom

  def max(that: Rational) = if (this.less(that)) that else this

  override def toString = numer + "/" + denom
}


type Set = Int => Boolean

/**
  * Indicates whether a set contains a given element.
  */
def contains(s: Set, elem: Int): Boolean = s(elem)

/**
  * Returns the set of the one given element.
  */
def singletonSet(elem: Int): Set = (i: Int) => i == elem


/**
  * Returns the union of the two given sets,
  * the sets of all elements that are in either `s` or `t`.
  */
def union(s: Set, t: Set): Set = (i: Int) => s(i) || t(i)

/**
  * Returns the intersection of the two given sets,
  * the set of all elements that are both in `s` and `t`.
  */
def intersect(s: Set, t: Set): Set = (i: Int) => s(i) && t(i)

/**
  * Returns the difference of the two given sets,
  * the set of all elements of `s` that are not in `t`.
  */
def diff(s: Set, t: Set): Set = (i: Int) => s(i) && !t(i)

/**
  * Returns the subset of `s` for which `p` holds.
  */
def filter(s: Set, p: Int => Boolean): Set = intersect(s, p)

def positive = (x: Int) => x > 0
def even = (x: Int) => x % 2 == 0

positive(3)
even(4)
union(even, positive)(1)
intersect(even, positive)(2)
filter(even, x => x > 5)(6)

val bound = 1000

def forall(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (a > bound) true
    else if (diff(s, p)(a)) false
    else iter(a + 1)
  }
  iter(-bound)
}

forall(even, x => x % 2 == 0)

def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, i => !p(i))

exists(even, x => x % 2 != 0)

def toString(s: Set): String = {
  val xs = for (i <- -bound to bound if contains(s, i)) yield i
  xs.mkString("{", ",", "}")
}
def printSet(s: Set) {
  println(toString(s))
}

printSet(even)











