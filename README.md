# june-lang

a small statically-typed language that runs on wasm

## Features

- [ ] static typing: programs are checked during compilation
- [ ] basic types: int, float, bool, string
- [ ] the usual arithmetic and logical operators
- [ ] user-defined structs
- [ ] common collections: maps, sets, vecs
- [ ] first-class functions with closure
- [ ] garbage collection
- [ ] file i/o

## Non-features

- subtyping
- generics for user-defined types

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

This is my seventh language implementation. Here are the previous ones:

-   [parents](https://github.com/dhconnelly/parents): A bytecode VM and
    compiler for a dynamic language. Implemented in TypeScript. Manages its own
    heap and implements mark-and-sweep garbage collection rather than
    delegating to the JS runtime. Supports first-class functions with closure
    and uses this to implement lists and so on.
-   [june-old](https://github.com/dhconnelly/june-old) (incomplete): A bytecode
    VM and compiler for a Scheme variant. Implemented in C++. Only compiles and
    evaluates conditionals and assignment.
-   [ungulate](https://github.com/dhconnelly/ungulate): An interpreter for a
    Scheme variant, written in OCaml.
-   [crab](https://github.com/dhconnelly/crab) (incomplete): A bytecode VM and
    compiler for a simple dynamic language. Implemented in Rust. I never
    implemented garbage collection and assignment just copies entire values.
-   [yalig](https://github.com/dhconnelly/yalig): Another interpreter for a
    Scheme variant, written in Go.
-   [ts](https://github.com/dhconnelly/ts): An extremely simple interpreter
    implemented in C++.

I read a lot of stuff that made this possible. Here are the resources that
helped me the most:

-   [Crafting Interpreters](https://craftinginterpreters.com/)
-   [Writing A Compiler In Go](https://compilerbook.com/)
-   [Writing An Interpreter In Go](https://interpreterbook.com/)
-   [My First Language Frontend with LLVM Tutorial](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html)

Additionally, I've read most of SICP and poked around in the Engineering
Compilers and Modern Compiler Implementation in ML textbook.

I also briefly worked on the GCL team at Google. (GCL is a bizarre internal
language used to generate protocol buffers in a whole bunch of different
scenarios, including configuration of Borg jobs as well as live experiments;
there are some more details in [this
paper](https://research.tue.nl/en/studentTheses/gcl-viewer)). For various
reasons I didn't stay long enough to have any impact, but I was lucky to have
the opportunity to review a bunch of code written by the team for a new
implementation of the language, including some of the bytecode VM and compiler
as well as garbage collection. This was a good (if brief) learning experience.

## License

MIT
