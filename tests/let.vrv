let a = 1 {
  print(a)
}

fn expensive() -> string {
  print("expensive")
  "cheap"
}

// should call expensive() only once
let cheap = expensive() {
  print(cheap)
  print(cheap)
}

// should not cache value as scope - should access directlyfrom stack
fn foo(n: int) -> void {
  let n_ = n {
    print(n_)
  }
}

foo(1)
foo(2)

// inner let should access parents' scope
let a = 1 {
  let b = 2 {
    foo(a + b)
  }

  let c = 7 {
    foo(a + c)
  }
}
