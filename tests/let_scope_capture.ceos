type foo { Foo(int) }

fn bar(foo: foo) -> () -> void {
  fn _() -> void {
    let Foo(x) = foo {
      print(x)
    }
  }
}

fn baz(foo: foo) -> () -> void {
  let Foo(x) = foo {
    fn _() -> void {
      print(x)
    }
  }
}

let f = bar(Foo(42)) {
  f()
}

let f = baz(Foo(42)) {
  f()
}
