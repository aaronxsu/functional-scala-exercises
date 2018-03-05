object FibonacciNumber {
    def fib(n: Int): Int = {
        @annotation.tailrec
        def loop(n: Int, first: Int, second: Int): Int = {
            if (n == 0) first
            else loop(n - 1, second, first + second)
        }
        loop(n, 0, 1)
    }

    def main(args: Array[String]): Unit = {
      println(fib(5))
    }
}
