fn main() {
    for arg in std::env::args().skip(1) {
        if let Err(err) = june_lang::driver::execute_file(&arg) {
            eprintln!("junec: {}", err);
            std::process::exit(1);
        }
    }
}
