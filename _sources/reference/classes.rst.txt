Classes
=======

.. prismjs:: verve

  class C {
    let field : Type

    fn method() -> Type {
      self.field
    }
  }

Although Verve has classes, they are not like classes you would find in object-oriented languages such as Java. In Verve, classes are a convenience for specifying a relationship between some data and functions, but they are still immutable, all fields are public and methods are just regular functions that take ``self`` as the first argument.

Background
----------

The class C above is syntactic sugar for:

.. prismjs:: verve

  enum C {
    C { field: Type }
  }

  fn method(self: C) -> Type {
    match self {
      case C(self):
        self.field
    }
  }
