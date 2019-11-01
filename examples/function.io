fn bar(first: str, second: str, third: bool) -> i32 {
    if (!third) {
        print first;
    } else {
        print second;
    }

    return 42;
}

fn foo(arg: i32) {
    var returned = bar("first", "second", arg > 5);
    print returned;
}

foo(3);
