// Should fail since the type of x is not bool
fn bar() {
    var y = 5 + 2;
    var x = 4 + y;
    if (x) print "hello";
}
