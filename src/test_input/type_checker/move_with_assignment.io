// Should fail since the print statement uses x but it was moved into y.

move()
  var x = 3
  var y = 5
  y = x
  print x

move()
