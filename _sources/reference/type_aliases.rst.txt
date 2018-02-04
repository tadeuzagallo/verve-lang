Type Aliases
============

As types get more complicated, it can be become cumbersome to have to repeat long types all over your code, and it often results in code that is hard to read and understand.

Just like it's good practice to have constants instead of "magic numbers" all over your code, it can be useful to give names to types too.

.. prismjs:: verve

  type Callback = (Int) -> Int

  fn doSomething(callback: Callback) { /* ... */ }
