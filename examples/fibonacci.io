// Prints the fibonacci numbers until `up_to`
// and returns the last one that was calculated
fibonacci(up_to: i32) -> i32

    let fib1 = 0
    let fib2 = 1

    let next = 0

    while next <= up_to

        print next

        fib1 = fib2
        fib2 = next
        next = fib1 + fib2

    return next

main()
  print fibonacci(100)
