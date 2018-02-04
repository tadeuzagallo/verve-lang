Match
=====

In order to use an enum value we need to determine which option was used to create it. That is done using a :code:`match` expression.

Using the example enum from last page we can implement a function :code:`area`.

.. prismjs:: verve

  fn area(shape: Shape) -> Int {
    match shape {
      case Square(side): side * side
      case Rectandle(width, height): width * height
    }
  }
