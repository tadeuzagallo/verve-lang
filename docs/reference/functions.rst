Functions
=========

Functions are defined using the keyword :code:`fn` and must have explicit type annotations.

.. prismjs:: verve

  fn double(x: Int) -> Int {
    x * x
  }

Functions can be called using C-style function calls:

.. prismjs:: verve

  double(42)

Currying
--------

All the functions are automatically curried, which means that both of the following are equivalent.

.. prismjs:: verve

  fn add(x: Int, y: Int) -> Int {
    x + y
  }

  fn add(x: Int) -> (Int) -> Int {
    fn (y: Int) -> Int {
      x + y
    }
  }

Both of the :code:`add` functions defined above can be called in any of the following ways.

.. prismjs:: verve

  add(3, 5)

  add(3)(5)

  let add3 = add(3)
  add3(5)
