fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("usage: junec file");
        std::process::exit(1);
    };
    if let Err(err) = june_lang::compiler::compile(&args[1]) {
        eprintln!("junec: {}", err);
        std::process::exit(1);
    }
}
