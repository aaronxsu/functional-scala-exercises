/*
  here is the trace given the question:
    foldRight(List(1, 2, 3), Nil)(Cons(_, _))
    Cons(1, foldRight(List(2, 3), Nil)(Cons(_, _)))
    Cons(1, Cons(2, foldRight(List(3, Nil), Nil)(Cons(_, _))))
    Cons(1, Cons(2, Cons(3, foldRight(List(Nil), Nil)(Cons(_, _)))))
    Cons(1, Cons(2, Cons(3, Nil)))
    which is just List(1, 2, 3) based on its definition
*/
