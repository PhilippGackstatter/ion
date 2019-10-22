extern crate ion;
use std::env;
use std::fs::File;
use std::io::Read;

fn main() {
    let path = env::args()
        .nth(1)
        .expect("Please specify the path to a file as the 1st arg.");

    // Specify anything as the 2nd arg to enable debug mode
    let run_until = if let Some(until) = env::args().nth(2) {
        Some(
            until
                .parse()
                .unwrap_or_else(|_| panic!("Provide a number as argument.")),
        )
    } else {
        None
    };

    let full_path = env::current_dir().unwrap().join(path);
    println!("Opening {:?}", full_path);
    let mut file = File::open(full_path).unwrap();
    let mut file_buf = Vec::new();
    file.read_to_end(&mut file_buf).unwrap();
    let input = String::from_utf8(file_buf)
        .unwrap_or_else(|_| panic!("Please provide a valid UTF-8 encoded file."));

    if let Some(till) = run_until {
        ion::util::run(input, till);
    } else {
        ion::util::run(input, 5);
    }
}
