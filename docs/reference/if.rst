If
==

There's nothing very special about `if`\ s in Verve:

.. prismjs:: verve

  if is_true {
    print("true")
  } else {
    print("false")
  }

The if conditional value (immediately after `if`) need not be quoted and the body must always be wrapped in curly braces. In Verve, `if` is an expression, which means that it can be used anywhere where you could use a variable.

.. prismjs:: verve

  print(if is_true {
    "true"
  } else {
    "false"
  })
