extern crate argparse;
extern crate ion;
use argparse::Store;
use ion::util::Options;
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

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

    let path: &Path = file_path.as_ref();
    let full_path = env::current_dir().unwrap().join(path);
    let mut file = File::open(full_path).unwrap();
    let mut file_buf = Vec::new();
    file.read_to_end(&mut file_buf).unwrap();

    let input = String::from_utf8(file_buf)
        .unwrap_or_else(|_| panic!("Please provide a valid UTF-8 encoded file."));

    ion::util::run(input, &opt);
}
