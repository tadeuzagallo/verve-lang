Type Aliases
============

As types get more complicated, it not just becomes annoying to have to type really long type signatures, but it can also become hard to read the code and understand what those types actually refer to.

Just like we avoid magic numbers, it can be useful to give names to types.

.. code-block:: rust

  type Callback = (Int) -> Int

  fn doSomething(callback: Callback) { /* ... */ }
