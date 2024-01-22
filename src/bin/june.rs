use std::path::Path;

fn main() {
    for arg in std::env::args().skip(1) {
        let path = Path::new(&arg);
        if let Err(err) = june_lang::driver::evaluate_file(&path) {
            eprintln!("june: {}", err);
            std::process::exit(1);
        }
    }
}
