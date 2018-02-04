Interfaces
==========

Creating a new Interface
------------------------

.. prismjs:: verve

  interface Shape<T> {
    let area : (T) -> Int
    let perimeter : (T) -> Int
  }


Conforming to an Interface
--------------------------

.. prismjs:: verve
  
  implementation Shape<Square> {
    fn area(s: Square) -> Int {
      s.side * s.side
    }

    fn perimeter(s: Square) -> Int {
      s.side * 4
    }
  }

Interfaces and Generics
-----------------------

.. prismjs:: verve

  fn printArea(s: Shape) { /* ... */ }
  //=> TypeError: Unknown type: Shape

As it turns out, ``Shape`` is not a type, it's an interface. In order to write a function that accepts any shape you need a type that conforms to the ``Shape`` interface.

.. prismjs:: verve

  fn printArea<T: Shape>(s: Shape) {
    print(s.area())
  }
