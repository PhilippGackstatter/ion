fn bar(first, second, third) {
    first = "Modified!";
    print first;
    print second;
    print third;
}

fn foo(myglob) {
    var myarg = "Hello!";
    bar(myarg, myglob, myarg);
}

var myglob = "global!";
foo(myglob);
