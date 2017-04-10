=====
Verve
=====

Records

::

  { field: Type }

Enums

::

  enum Optional<T> {
    None()
    Some(T)
  }

  enum List<T> {
    None()
    Some(T, List<T>)
  }

  enum Nat {
    Z()
    S(Nat)
  }

GADTs

::

  enum Term<_> {
    Int(Int) -> Term<Int>
    Add() -> Term<(Int, Int) -> Int>
    App<T, U>(Term<T -> U>, Term<T>) -> Term<U>
  }

Classes:
co- and contra-variance not necessary (?)

::

  class List<T> {
    items: List'<T>

    // can't capture T
    fn append<T>(self: List<T>, x: T) -> List<T> {
      ...
    }
  }

Functions

::

  fn <name>(<params>) -> <type> { <body> }

Examples:

::

  fn id<T>(x: T) -> T { x }

  fn add<T: Num>(x: T, y: T) -> T { x + y }

  fn fst<T, U>(Pair<T, U>(t, u)) -> T { t }

  fn snd<T, U>(Pair<T, U>(t, u)) -> U { u }

  fn m<T, U, V>(x: Either<T, U>, l : (T) -> V, r : (U) -> V) -> V {
    match x {
      Left(a) => l(a)
      Right(b) => r(b)
    }
  }

Function applications

::
  
  fst<A,B>(x)

  f(x, y) = f(x)(y) /= f((x, y))

Records are first class values and types, ideally with depth, width and permutation subtyping

::

  fn pair<T, U, V>({fst, snd}: { fst: T, snd: U }, b: (T, U) -> V) -> V {
    b(fst, snd)
  }


.. _Import statements:

Import statements: the import statements must be the first thing in the file.

::

  import <what> from <where> as <alias>

`<what>`: either `*` or a record of names to be imported.
`<where>`: the location of the file
`<alias>`: (optional) Import `<what>` into a custom namespace `<alias>`.

Examples:

::

  import * from './foo.vrv'

  // import selected functions into global namespace
  import { foo, bar } from './foo.vrv'

  // import the whole module as Foo
  import * from './foo.vrv' as Foo

  // import selected functions as Foo
  import { foo, bar } from './foo.vrv' as Foo

  // import the whole module but hide a few functions
  import * except { foo } from './foo.vrv'

Export statements:
  The contents of a file are private by default, and anything that should be accessible from an external module must be exported explicitly.

  Export statements should be the second thing on a file, only preceded by `Import statements`_

::

  exports * // exports the whole module
  exports { foo, bar } // exports foo and bar

.. _exceptions:

Exceptions:

Declaring a new exception

::

  exception NotFound {
    key: String
    foo: Bar
  }

Exception handling

::

  try {

    throw NotFound(key: "foo")

  } catch NotFound(key) {
    print("Couldn't find ${key}")
  }
