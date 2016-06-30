type foo {
  Bar(int, int)
}

fn _(a: foo) -> void { 
  let Bar(x, y) = a {
    print(x + y)
  }
} (Bar(1, 2))

type list_<t> {
  Nil()
  Cons(t, list_<t>)
}

fn print_list(l: list_<stringify>) -> void {
  match l {
    Nil() => {}
    Cons(x, rest) => {
      print(x)
      print_list(rest)
    }
  }
}

print_list(Cons(1, Cons(2, Cons(3, Nil()))))
