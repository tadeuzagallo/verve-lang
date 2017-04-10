GADTs example (Extracted from the `OCaml docs`_)

.. _OCaml docs: http://

::

  enum Term<_> {
    Int(Int) -> Term<Int>
    Add() -> Term<(Int, Int) -> Int>
    App<T, U>(Term<T -> U>, Term<T>) -> Term<U>
  }

  fn eval<T>(term: Term<T>) -> T {
    match term with {
      Int(i) -> i
      Add() -> fn (a, b) { a + b }
      App(fn, v) -> fn(v)
    }
  }
