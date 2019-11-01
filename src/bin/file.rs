extern crate argparse;
extern crate ion;
use argparse::Store;
use ion::util::Options;

fn main() {
    let mut file_path = String::new();
    let mut opt = Options::new();

    {
        let mut parser = ion::util::get_repl_parser(&mut opt);
        parser
            .refer(&mut file_path)
            .add_argument("file", Store, "The ion file to execute.");
        parser.parse_args_or_exit();
    }

    let input = ion::util::file_to_string(&file_path);

    ion::util::run(input, &opt);
}
