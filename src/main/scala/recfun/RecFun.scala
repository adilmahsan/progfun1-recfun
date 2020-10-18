package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
    println("())(")
    println(balance(":-)".toList))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0 || r == c)
      {
        return 1
      }
      else
      {
        return (pascal(c-1, r-1) + pascal(c, r-1))
      }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def isBalanced(chars: List[Char], count: Int): Boolean = {
      if (count < 0)
        return false;
      else if (chars.isEmpty)
        return (count == 0)
      else if (chars.head == '(')
        return isBalanced(chars.tail, count + 1)
      else if (chars.head == ')')
        return isBalanced(chars.tail, count - 1)
      else
        return isBalanced(chars.tail, count)
    }
    var count = 0;
    var result = isBalanced(chars, count)
    return result

  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
