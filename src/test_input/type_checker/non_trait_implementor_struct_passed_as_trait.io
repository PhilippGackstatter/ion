// Should fail since Point does not implement Display.

trait Display
  fmt(self) -> str

struct Point
  x: i32
  y: i32

takesTrait(arg: Display)
  arg.fmt()

testFunc()
  var point = Point { x: 0, y: 1,}
  takesTrait(point)
