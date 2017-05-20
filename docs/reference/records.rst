Records
=======

Records provide a more flexible way of dealing with data.

.. code-block:: rust

  fn f(x: { title: String, description: String }) {
    print(x.title)
    print(x.description)
  }

  f({ title: "...", description: "..." })

  f({ title: "...", description: "...", price: ... })
