Relation between Class and Records

::
  
  class X { x: Int } <: { x: Int } <: {}
  class Y { x: Int } <: { x: Int } <: {}
  class Z { x: Int; y: String } <: {x:Int; y: String} <: { x: Int } <: {}

  fn f({ x: Int }) { x }
  fn g({ x: Int }: X) { x }

  let x = X(x: 42)

  f(x)
  g(x)
