extern crate ion;
use std::env;
use std::io;
use std::io::prelude::*;

fn main() {
    // Specify anything as the 1st arg to enable debug mode
    let debug_enabled = env::args().nth(1).is_some();

    println!("ion v0.1.0");
    loop {
        print!(">> ");
        io::stdout().flush().expect("Could not flush stdout");

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                if debug_enabled {
                    ion::util::lexit(input);
                } else {
                    ion::util::run(input);
                }
            }
            Err(error) => println!("error: {}", error),
        }
    }
}
