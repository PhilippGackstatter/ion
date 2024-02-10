// Should fail since the implemented function name differs from the trait.

trait Display
  fmt(self) -> str

struct Point
  x: i32
  y: i32

impl Display for Point
  other(self, arg:i32) -> str
    return ""
