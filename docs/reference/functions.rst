Functions
=========

Functions are defined using the keyword ``fn`` and must have explicit type annotations.

.. prismjs:: verve

  fn double(x: Int) -> Int {
    x * 2
  }

Functions can be called using C-style function calls:

.. prismjs:: verve

  double(42)

Void Functions
--------------

Functions that do not return a value can omit the return type instead of explicitly saying ``Void``.

.. prismjs:: verve

  fn ignore<T>(x: T) {
    // evaluates whatever is passed into `x`, but ignores it's result
  }

Currying
--------

All the functions are automatically curried, which means that both of the following are equivalent.

.. prismjs:: verve

  fn add(x: Int, y: Int) -> Int {
    x + y
  }

  fn add(x: Int) -> (Int) -> Int {
    fn add'(y: Int) -> Int {
      x + y
    }
  }

Both of the ``add`` functions defined above can be called in any of the following ways.

.. prismjs:: verve

  add(3, 5)

  add(3)(5)

  let add3 = add(3)
  add3(5)

Universal Function Call Syntax (UFCS)
-------------------------------------

UFCS allows you to call a function as if was a method of its first argument. Let's see an example:

.. prismjs:: verve

  fn bool_to_int(x: Bool) -> Int {
    if x { 1 } else { 0 }
  }

  bool_to_int(False) //=> 0 : Int

  // Alternatively

  True.bool_to_int() //=> 1 : Int

This makes it possible to "extend" a class after its definition.

.. prismjs:: verve

  class C { /* ... */ }

  let c = C({})

  fn printC(self: C) {
    // ...
  }

  c.printC()

And behaves similar to the reverse application operator in other functional languages like Elm_, `F#`_ and OCaml_ (and recently proposed to JavaScript_):

.. _Elm: http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#|>
.. _F#: https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/operators.%5b-h%5d-%5d%5b't1,'u%5d-function-%5bfsharp%5d
.. _OCaml: http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html#VAL%28%7C%3E%29
.. _JavaScript: https://github.com/tc39/proposal-pipeline-operator

.. prismjs:: verve

  list.map(f).filter(g).sort(h).iter(i)

is equivalent to the following in Elm for example:

.. prismjs:: verve

  list |> map f |> filter g |> sort h |> iter i

with the difference that with UFCS the object is passed as the first argument and with the reverse application operator it would be passed as the last argument.
