Generics
========

.. prismjs:: verve

  fn mapInt(fun: (Int) -> Int, list: IntList) -> IntList {
    // ...
  }

.. prismjs:: verve

  fn mapFloat(fun: (Float) -> Float, list: FloatList) -> FloatList {
    // ...
  }

.. prismjs:: verve

  fn map<T>(fun: (T) -> T, list: List<T>) -> List<T> {
    // ...
  }

  map<Int, Int>(increment, [1, 2, 3])

.. prismjs:: verve

  fn map<T, U>(fun: (T) -> U, list: List<T>) -> List<U> {
    // ...
  }

  map<Int, Int>(increment, [1, 2, 3]) //=> [2, 3, 4] : List<Int>
  map<Int, Bool>((>)(2), [1, 2, 3]) //=> [False, False, True] : List<Bool>
