Comments
========

Verve has C-style comments, with :code:`//` for single line comments and :code:`/* */` for multiline comments.

For single line comments, between the :code:`//` and the end of the line will be ignored:

.. prismjs:: verve

  // this is a single line comment

  x // this is a comment about x

Multiline comments, despite the name, need not be multiline. Everything between the opening (:code:`/*`) and closing (:code:`*/`) will be ignored.

.. prismjs:: verve

  /*
    This can be a very long multiline comment
  */

  x /* or single line */ + y
