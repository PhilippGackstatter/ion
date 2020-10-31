extern crate ion;
use ion::util::Options;
use std::io;
use std::io::prelude::*;

fn main() {
    let mut opt = Options::new();

    {
        let parser = ion::util::get_cli_parser(&mut opt);
        parser.parse_args_or_exit();
    }

    println!("ion v0.1.0");
    loop {
        print!(">> ");
        io::stdout().flush().expect("Could not flush stdout");

        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                ion::util::run(input, &opt);
            }
            Err(error) => println!("error: {}", error),
        }
    }
}
