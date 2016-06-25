type maybe<t> {
  Just(t)
  Nothing()
}

interface monad<m> {
  virtual return <t>(t) -> m<t>
  virtual bind <t, u>(m<t>, (t) -> m<u>) -> m<u>
}

implementation monad<maybe> {
  fn return(x) {
    Just(x)
  }

  fn bind(maybe, f) {
    match maybe {
      Nothing() => Nothing()
      Just(x) => f(x)
    }
  }
}

fn print_maybe(maybe: maybe<stringify>) -> void {
  match maybe {
    Nothing() => print("Nothing")
    Just(x) => print(x)
  }
}

print_maybe(Just(5))
print_maybe(Nothing())

fn square(n: int) -> maybe<int> {
  Just(n * n)
}

print_maybe(bind(bind(Just(3), square), square))
