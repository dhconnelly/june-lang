use june_lang;
use std::env;
use std::error;
use std::fmt;
use std::fs::File;
use std::io;
use std::io::BufReader;
use std::result;

struct Program;

#[derive(Debug)]
enum Error {
    IO(io::Error),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::IO(err)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "error!")
    }
}

impl error::Error for Error {}

type Result<T> = result::Result<T, Error>;

impl Program {
    fn run(&self) -> Result<()> {
        for path in env::args().skip(1) {
            let f = File::open(path)?;
            let scanner = june_lang::scanner::scan(BufReader::new(f));
            let toks: result::Result<Vec<_>, _> = scanner.collect();
            println!("{:?}", toks);
        }
        Ok(())
    }

    fn from_env() -> Result<Program> {
        Ok(Program)
    }
}

fn die(err: impl error::Error) -> ! {
    std::eprintln!("{}", err);
    std::process::exit(1);
}

fn main() {
    let prog = match Program::from_env() {
        Ok(prog) => prog,
        Err(err) => die(err),
    };
    if let Err(err) = prog.run() {
        die(err);
    }
}
