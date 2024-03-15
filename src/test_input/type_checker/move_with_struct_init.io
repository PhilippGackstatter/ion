// Should fail since the print statement uses x but it was moved into the struct field.

struct Test
  num: i32

move()
  var x = 3
  var test = Test { num: x, }
  print x

move()