::

  // Ad hoc polymorphism
  interface Shape<T> {
    fn area(T) -> Float
  }

  class Square { side: Float }
  class Circle { radius: Float }

  implementation Shape<Square> {
    fn area(square: Square) -> Float {
      pow(square, 2)
    }
  }

  implementation Shape<Circle> {
    fn area(circle: Circle) -> Float {
      Math.pi * Math.pow(circle.radius, 2)
    }
  }

  let a = Square(side: 2).area() // 4 : Int
  let b = Circle(radius: 3).area() // 28 : Int
