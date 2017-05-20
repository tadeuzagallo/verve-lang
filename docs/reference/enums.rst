Enums
=====

Enums in Verve are a little bit different from C-style enums. Instead of being only a tag, it can also contain values attached to each option.

.. code-block:: swift

  enum Shape {
    Square(Int) // side
    Rectangle(Int, Int) // width x height
  }

  fn f(x: Shape) { /* ... */ }

  f(Square(10))

  f(Rectangle(5, 10))
