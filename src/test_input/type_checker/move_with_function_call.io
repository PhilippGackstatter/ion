// Should fail since the print statement uses x but it was moved into the function call.

addOne(num: i32) -> i32
  return num + 1

move()
  var x = 3
  addOne(x)
  print x

move()
