Variables
=========

Variables are defined using the `let` keyword. Variable names must begin with a lowercase letter or an underscore (`_`).

.. prismjs:: verve

  let var = 13

Variables are immutable, and there is no syntax for changing the value of a variable. It's possible however to shadow variables both locally and in an inner scope.

.. prismjs:: verve

  let x = 42
  print(x) // 42
  let x = 13
  print(x) // 13

  if True {
    let x = 7
    print(x) // 7
  }

  print(x) // 13
