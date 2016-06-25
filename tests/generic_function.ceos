fn id<t>(a: t) -> t {
  a
}

print(id("foo"))
print(id(42))

interface foo<t> {
  virtual bar<u> (t, u) -> u
}

implementation foo<string> {
  fn bar(str, x) {
    print(str)
    x
  }
}

print(bar("foo", 42))
