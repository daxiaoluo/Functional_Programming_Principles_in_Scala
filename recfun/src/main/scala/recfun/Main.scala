package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if(c == r) 1
    else if(c == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceAssist(chars: List[Char], num: Int): Int = {
     if(chars.isEmpty || num < 0) num
      else if(chars.head == '(') balanceAssist(chars.tail, num + 1)
      else if(chars.head == ')') balanceAssist(chars.tail, num - 1)
      else balanceAssist(chars.tail, num)
    }
    balanceAssist(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) 1
    else if(money < 0) 0
    else if(coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
