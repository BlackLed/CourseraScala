package recfun

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
    def pascal(c: Int, r: Int): Int =  {
      if (c == 0 && r == 0)  1
      else if (c < 0 || r < 0)  0
      else pascal(c-1,r-1)+pascal(c,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(charsO: List[Char]): Boolean = {
      def checkBalance(chars: List[Char], seen: Int): Boolean = {
        if(chars.isEmpty) seen == 0
        else if(chars.head == ')' && seen == 0) false
        else if(chars.head == '(') checkBalance(chars.tail, seen+1)
        else if(chars.head == ')') checkBalance(chars.tail, seen-1)
        else false
      }
      checkBalance(charsO.filter( c=> c == ')' || c == '('), 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countChangeImpl(coins: List[Int], amount: Int): Int = {
        if(amount == 0) 1
        else if (amount < 0 || coins.isEmpty) 0
        else countChangeImpl(coins, amount-coins.head) + countChangeImpl(coins.tail, amount)
      }

      countChangeImpl(coins,money)
    }
  }
