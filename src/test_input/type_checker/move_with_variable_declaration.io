// Should fail since the print statement uses x but it was moved into y.

move()
  let x = 3
  let y = x
  print x
