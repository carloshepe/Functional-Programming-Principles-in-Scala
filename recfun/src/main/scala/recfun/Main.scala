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
    if (c == 0 || r == 1 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceLoop(chars: List[Char], flag: Int): Boolean = {
      if (flag < 0)
        false
      else if (!chars.isEmpty) {
        if (chars.head == '(') balanceLoop(chars.tail, flag + 1)
        else if (chars.head == ')') balanceLoop(chars.tail, flag - 1)
        else balanceLoop(chars.tail, flag)
      }
      else flag == 0
    }
    balanceLoop(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loopCountChange(money: Int, coins: List[Int], acc: Int): Int = {
      if (money < 0 || coins.isEmpty) 0
      else if (money == 0) acc + 1
      else loopCountChange(money, coins.tail, acc) + loopCountChange(money - coins.head, coins, acc)
    }
    if (money < 1) 0 else loopCountChange(money, coins, 0)
  }
}
