use std::path::Path;

fn main() {
    for arg in std::env::args().skip(1) {
        let path = Path::new(&arg);
        if let Err(err) = june_lang::driver::compile_file(&path) {
            eprintln!("junec: {}", err);
            std::process::exit(1);
        }
        if let Err(err) = june_lang::driver::execute(&path.with_extension("wasm")) {
            eprintln!("junec: {}", err);
            std::process::exit(1);
        }
    }
}
