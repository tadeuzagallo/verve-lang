Generics
========

.. prismjs:: verve

  fn mapInt(list: IntList, fun: (Int) -> Int) -> IntList {
    // ...
  }

.. prismjs:: verve

  fn mapFloat(list: FloatList, fun: (Float) -> Float) -> FloatList {
    // ...
  }

.. prismjs:: verve

  fn map<T>(list: List<T>, fun: (T) -> T) -> List<T> {
    // ...
  }

  map<Int, Int>([1, 2, 3], increment)

.. prismjs:: verve

  fn map<T, U>(list: List<T>, fun: (T) -> U) -> List<U> {
    // ...
  }

  map<Int, Int>([1, 2, 3], increment) //=> [2, 3, 4] : List<Int>
  map<Int, Bool>([1, 2, 3], (>)(2)) //=> [False, False, True] : List<Bool>
