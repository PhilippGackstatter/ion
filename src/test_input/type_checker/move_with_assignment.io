// Should fail since the print statement uses x but it was moved into y.

move()
  let x = 3
  let y = 5
  y = x
  print x
