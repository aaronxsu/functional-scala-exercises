def isSorted[A] (as: Array[A], isGreater: (A, A) => Boolean): Boolean = {
    def loop(idx: Int): Boolean = {
        if (idx >= arr.length - 1) true
        else if (isGreater(as(idx), as(idx + 1))) false
        else loop(idx + 1)
    }
    loop(0)
}
