extern crate argparse;
extern crate ion;
use argparse::Store;
use ion::util::Options;
use std::io;
use std::io::prelude::*;

fn main() {
    let mut file_path = String::new();
    let mut opt = Options::new();

    {
        let mut parser = ion::util::get_cli_parser(&mut opt);
        parser
            .refer(&mut file_path)
            .add_argument("file", Store, "The ion file to execute.");
        parser.parse_args_or_exit();
    }

    if file_path == "" {
        println!("ion v{}", env!("CARGO_PKG_VERSION"));
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
    } else {
        let input = ion::util::file_to_string(&file_path);

        ion::util::run(input, &opt);
    }
}
