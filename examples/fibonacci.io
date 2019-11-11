// Prints the fibonacci numbers until `up_to`
// and returns the last one that was calculated
fn fibonacci(up_to: i32) -> i32 {

    var fib1 = 0;
    var fib2 = 1;

    var next = 0;

    while(next <= up_to) {

        print next;

        fib1 = fib2;
        fib2 = next;
        next = fib1 + fib2;

    }

    return next;

}

// The type of `last` can be inferred from the function signature
var last = fibonacci(100);
print last;
