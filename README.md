# june-lang

a small statically-typed language that runs on [wasm](https://webassembly.org/)

## TODO

end-to-end for basic programs:

- [x] scanner
- [x] parser
- [x] analyzer
- [ ] compiler
- [ ] wasm serializer
- [ ] make it work
- [ ] garbage collection

add features:

- [ ] recursion and mutual recursion
- [ ] forward references for globals
- [ ] return types
- [ ] assignment
- [ ] conditionals
- [ ] loops
- [ ] structs
- [ ] vecs
- [ ] maps
- [ ] sets
- [ ] file i/o

refactoring:

- [ ] avoid copies in ast
- [ ] static func type in ast
- [ ] just one From<io::Error> in scanner
- [ ] add back (line, col) tracking
- [ ] nice error reporting
- [ ] improve the algebra
- [ ] simplify the awful functions
- [ ] narrow public interface
- [ ] nicer main.rs w/structopt
- [ ] read and apply https://thume.ca/2019/04/18/writing-a-compiler-in-rust/

productionization:

- [ ] skim relevant parts of engineering a compiler
- [ ] skim relevant parts of types and programming languages

## Purpose

I wanted to write a compiler that targets a VM that I didn't create myself, and
I wanted to implement static type checking, neither of which I had done before.
The design is roughly "Go but even simpler" and the feature set corresponds to
what I think is necessary to solve Advent of Code puzzles.

## Features

- builds binaries that can be executed by [wasmtime](https://wasmtime.dev)
- static typing: programs are checked during compilation
- basic types: int, float, bool, string
- the usual arithmetic and logical operators, loops, conditionals
- recursion, mutual recursion, forward references for globals
- user-defined structs
- common collections: maps, sets, vecs
- garbage collection
- file i/o

## Non-features

- subtyping
- generics for user-defined types
- concurrency

## Example

    TODO

## Installation

Dependencies:

- [Rust](https://www.rust-lang.org/) (only if building from source)
- [wasmtime](https://wasmtime.dev/) to run compiled programs

To build from source:

    git clone git@github.com:dhconnelly/june-lang.git
    cargo test
    cargo install --path .

To install from crates.io:

    cargo install june-lang

To use a pre-built release:

    TODO

## Usage

    junec FILE
    wasmtime FILE.wasm

For more usage information, run `junec --help`. For examples, see `examples/`.

## Previous work and references

For more details on wasm, wasi, and wasmtime:

-   [WebAssembly Specification](https://webassembly.github.io/spec/core/)
-   [Wasmtime documentation](https://docs.wasmtime.dev/)
-   [WASI Document Guide](https://github.com/bytecodealliance/wasmtime/blob/main/docs/WASI-documents.md)

This is my eighth hobby language implementation. Here are the previous ones:

-   [parents](https://github.com/dhconnelly/parents): A bytecode VM and
    compiler for a dynamic language. Implemented in TypeScript. Manages its own
    heap and implements mark-and-sweep garbage collection rather than
    delegating to the JS runtime. Supports first-class functions with closure
    and uses this to implement lists and so on. This was my first experience
    implementing garbage collection.
-   [june-old](https://github.com/dhconnelly/june-old) (incomplete): A bytecode
    VM and compiler for a Scheme variant. Implemented in C++. Only compiles and
    evaluates conditionals and assignment.
-   [ungulate](https://github.com/dhconnelly/ungulate): An interpreter for a
    Scheme variant, written in OCaml.
-   [crab](https://github.com/dhconnelly/crab) (incomplete): A bytecode VM and
    compiler for a simple dynamic language. Implemented in Rust. I never
    implemented garbage collection and assignment just copies entire values.
    This was my first experience writing a compiler from scratch.
-   [yalig](https://github.com/dhconnelly/yalig): Another interpreter for a
    Scheme variant, written in Go.
-   [adso](https://github.com/dhconnelly/adso-js): A very simple interpreter
    implemented in JavaScript.
-   [ts](https://github.com/dhconnelly/ts): Another very simple interpreter
    implemented in C++. This was my first experience writing an interpreter
    from scratch.

None of these languages are interesting: it's all Scheme variants or JavaScript
lookalikes. June is my first attempt at creating a statically typed language.
Despite having used a lot of languages -- I've written C, C++, Objective-C,
Java, Scala, JavaScript, TypeScript, Python, and Go professionally (not
counting domain-specific languages), plus Rust, Clojure, Haskell, OCaml, and
Scheme for hobby stuff -- I'm less interested in language design than
implementation :)

I read a lot of stuff that made this possible. Here are the resources that
helped me the most:

-   [Crafting Interpreters](https://craftinginterpreters.com/)
-   [Writing A Compiler In Go](https://compilerbook.com/)
-   [Writing An Interpreter In Go](https://interpreterbook.com/)
-   [My First Language Frontend with LLVM Tutorial](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html)

Additionally, I've read most of SICP and poked around in the Engineering
a Compiler and Modern Compiler Implementation in ML textbook.

I also briefly worked on the GCL team at Google. (GCL is a bizarre internal
language used to generate protocol buffers in a whole bunch of different
scenarios, including configuration of Borg jobs as well as live experiments;
there are some more details in [this
paper](https://research.tue.nl/en/studentTheses/gcl-viewer)). For various
reasons I didn't stay long enough to have any impact, but I was lucky to have
the opportunity to review a bunch of code written by the team for a new
implementation of the language, including some of the bytecode VM and compiler
as well as garbage collection. This was a good (if short) learning experience.

## License

MIT
