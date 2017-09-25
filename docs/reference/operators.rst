Operators
=========

On top of functions, it is sometimes useful to define operators for common operations. Like :code:`+` and :code:`*` for infix addition and multiplication.

.. prismjs:: verve

  operator (x: Int) + (y: Int) -> Int {
    int_add(x, y)
  }

Operators are also curried function, and be called as a regular function by wrapping it in parentheses.

.. prismjs:: verve

  (+)(3, 5)

  (+)(3)(5)

  let add = (+)
  add(3, 5)

Associativity & Precedence
--------------------------

When defining an operator, you can use the attributes :code:`#prec` and :code:`#assoc` to specify.

.. prismjs:: verve

  #assoc(left) #prec(higher(+))
  operator (x: Int) * (y: Int) -> Int {
    int_mul(x, y)
  }
