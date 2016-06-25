type big_object {
  BigObject(string, string, string, string, string, string, string, string) // 8 strings
}

type pair {
  Pair(int, int)
}

type wrapper {
  Wrapper(pair)
}

fn allocate() -> big_object {
  BigObject(
    "foooooooooooooooooooooooooooooooooooooooooooo",
    "foooooooooooooooooooooooooooooooooooooooooooo",
    "foooooooooooooooooooooooooooooooooooooooooooo",
    "foooooooooooooooooooooooooooooooooooooooooooo",
    "foooooooooooooooooooooooooooooooooooooooooooo",
    "foooooooooooooooooooooooooooooooooooooooooooo",
    "foooooooooooooooooooooooooooooooooooooooooooo",
    "foooooooooooooooooooooooooooooooooooooooooooo"
  )
}

fn a() -> big_object { allocate() allocate() }
fn b() -> big_object { a() a() }
fn c() -> big_object { b() b() }
fn d() -> big_object { c() c() }
fn e() -> big_object { d() d() }
fn f() -> big_object { e() e() }
fn g() -> big_object { f() f() }
fn h() -> big_object { g() g() }
fn i() -> big_object { h() h() }
fn j() -> big_object { i() i() }
fn k() -> big_object { j() j() }
fn l() -> big_object { k() k() }
fn m() -> big_object { l() l() }
fn n() -> big_object { m() m() }

fn test(wrapped: wrapper) -> void {
  n() // allocate a lot and trigger garbage collection
  let Wrapper(pair) = wrapped {
    let Pair(a, b) = pair { 
      print([a, b])
    }
  }
}

test(Wrapper(Pair(42, 13)))

// this will allocate 4243744b (~4mb)
// the GC should get it down to ~20k
// here we just test it's less than ~30k
print(`__heap-size__`() < 30000)
