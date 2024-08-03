struct Rectangle
    width: i32
    height: i32


impl Rectangle

    area(self) -> i32
        return self.width * self.height

    // Associated method without self receiver.
    name() -> str
      return "rect"

main()
  let rect = Rectangle {
      width: 4,
      height: 3,
  }

  print Rectangle::name()
  print rect.area()
