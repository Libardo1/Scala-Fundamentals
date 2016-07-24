
List(2,1,4,6).sorted

val coins = List(2, 1)

def countChange(money: Int, coins: List[Int]): Int = {
  def f(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0
    else if (money - coins.head == 0) 1
    else if (money - coins.head < 0) 0
    else countChange(money - coins.head, coins) +
      countChange(money, coins.tail)
  }
  f(money, coins.sorted)
}

countChange(4, coins)








