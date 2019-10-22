extern crate ion;
use std::env;
use std::io;
use std::io::prelude::*;

fn main() {
    // Specify anything as the 1st arg to enable debug mode
    let run_until = if let Some(until) = env::args().nth(1) {
        Some(
            until
                .parse()
                .unwrap_or_else(|_| panic!("Provide a number as argument.")),
        )
    } else {
        None
    };

    println!("ion v0.1.0");
    loop {
        print!(">> ");
        io::stdout().flush().expect("Could not flush stdout");

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                if let Some(till) = run_until {
                    ion::util::run(input, till);
                } else {
                    ion::util::run(input, 5);
                }
            }
            Err(error) => println!("error: {}", error),
        }
    }
}
