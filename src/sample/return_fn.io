fn bar() {
    print 32;
}

fn foo(arg) {
    bar();
    var myarg = arg + 3;
    return myarg + 3;
}

var ret = foo(3);
print ret;
