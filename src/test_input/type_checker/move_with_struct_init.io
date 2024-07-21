// Should fail since the print statement uses x but it was moved into the struct field.

struct Test
  num: i32

move()
  let x = 3
  let test = Test { num: x, }
  print x
