// Should fail since the type of x is not bool
fn bar() {
    var y = 5 + 2;
    var x = y + 6;
    if (!!x) print "hello";
}
