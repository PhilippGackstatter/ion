extern crate argparse;
extern crate ion;
use argparse::Store;
use ion::util::Options;

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
        println!("Please provide a path to a file to execute.");
    } else {
        match ion::util::file_to_string(&file_path) {
            Ok(input) => ion::util::run(input, &opt),
            Err(err) => println!("{}", err),
        }
    }
}
