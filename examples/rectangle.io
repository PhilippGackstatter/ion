struct Rectangle
    width: i32
    height: i32


impl Rectangle

    area(self) -> i32
        return self.width * self.height

    // Static method without self receiver.
    name() -> str
      return "rect"


let rect = Rectangle {
    width: 4,
    height: 3,
}

print rect.name()
print rect.area()
