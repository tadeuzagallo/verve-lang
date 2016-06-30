interface stringify<t> {
  virtual to_string (t) -> string
}

implementation stringify<int> { }
implementation stringify<string> { }
implementation stringify<list<int>> { }
implementation stringify<list<string>> { }

extern print (stringify) -> void

extern count (string) -> int
extern substr (string, int) -> string
extern at (string, int) -> int

extern `+` (int, int) -> int
extern `-` (int, int) -> int
extern `*` (int, int) -> int
extern `/` (int, int) -> int
extern `%` (int, int) -> int

// should be bool
extern `&&` (int, int) -> int
extern `||` (int, int) -> int

extern `<` (int, int) -> int
extern `>` (int, int) -> int

extern `<=` (int, int) -> int
extern `>=` (int, int) -> int

extern `==` (int, int) -> int
extern `!=` (int, int) -> int

extern `unary_!` (int) -> int
extern `unary_-` (int) -> int

extern `__heap-size__` () -> int
