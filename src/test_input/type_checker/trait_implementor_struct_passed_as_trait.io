// Should succeed since Point implements Display.

trait Display
  fmt(self) -> str

struct Point
  x: i32
  y: i32

impl Display for Point
  fmt(self) -> str
    return "test"

takesTrait(arg: Display)
  arg.fmt()

testFunc()
  let point = Point { x: 0, y: 1,}
  takesTrait(point)
