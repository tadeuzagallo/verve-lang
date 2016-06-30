type maybe_int {
  JustInt(int)
  None()
}

fn print_maybe(a: maybe_int) -> void {
  match a {
    JustInt(x) => print(x)
    None() => print("None")
  }
}

print_maybe(JustInt(42))
print_maybe(None())
