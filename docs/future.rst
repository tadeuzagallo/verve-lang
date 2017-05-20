Under consideration
===================

GADTs
-----

.. code-block:: rust

  enum Term<_> {
    Int(Int) -> Term<Int>
    Add -> Term<(Int, Int) -> Int>
    App<T, U>(Term<T -> U>, Term<T>) -> Term<U>
  }

  fn eval<T>(term: Term<T>) -> T {
    match term with {
      Int(i) -> i
      Add() -> fn (a, b) { a + b }
      App(fn, v) -> fn(v)
    }
  }

Visibility
----------

Currently everything in the file is exported, which is not ideal. The current two approaches being considered are as follows.

Export declarations
+++++++++++++++++++

.. code-block:: rust

  export * // exports the whole module
  export { foo, bar } // exports foo and bar

Export by convention
++++++++++++++++++++

Names beginning with :code:`_` are private.

.. code-block:: rust

  fn _private() { /* ... */ } // _private won't be exported

  fn public() { /* ... */ } // public will be exported

This approach has the benefit that it could also be applied to class members visibility.

Exceptions
----------

Declaring a new exception

.. code-block:: rust

  exception NotFound {
    key: String,
    foo: Bar
  }


Exception handling

.. code-block:: rust

  try {

    throw NotFound(key: "foo")

  } catch NotFound(key) {
    print("Couldn't find ${key}")
  }
