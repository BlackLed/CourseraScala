
def factorial(x:Int):Int = {

  def compute(x: Int, rs: Int): Int =
    if (x == 0) rs else compute(x - 1, x * rs)

  compute(x,1)
}

factorial(4)
