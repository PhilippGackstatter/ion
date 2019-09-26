use std::env;
use std::fs::File;
use std::io::Read;

fn main() {
    let path = env::args()
        .nth(1)
        .expect("Please specify the path to a file as the 1st arg.");

    // Specify anything as the 2nd arg to enable debug mode
    // let debug_enabled = if let Some(_) = env::args().nth(2) {
    //     true
    // } else {
    //     false
    // };

    let mut file = File::open(path).unwrap();
    let mut file_buf = Vec::new();
    file.read_to_end(&mut file_buf).unwrap();
    let program_str = String::from_utf8(file_buf)
        .unwrap_or_else(|_| panic!("Please provide a valid UTF-8 encoded file."));
}
