# june-lang

a small statically-typed language that runs on wasm

## features

- [ ] static typing: programs are checked during compilation
- [ ] basic types: int, float, bool, string
- [ ] the usual arithmetic and logical operators
- [ ] user-defined structs
- [ ] common collections: maps, sets, vecs
- [ ] first-class functions with closure
- [ ] garbage collection
- [ ] file i/o

## non-features

- subtyping
- generics for user-defined types

## example

    TODO

## installation

Dependencies:

- [Rust](https://www.rust-lang.org/) (only if building from source)
- [wasmtime](https://wasmtime.dev/) to run compiled programs

To build from source:

    git clone git@github.com:dhconnelly/june-lang.git
    cargo check && cargo test && cargo clippy
    cargo install --path .
    junec examples/hello.june

To install from crates.io:

    cargo install june-lang
    wasmtime hello.wasm

To use a pre-built release:

    TODO

## usage

    junec FILE
    wasmtime FILE.wasm

For more usage information, run `junec --help`.

## previous work and references

This is my seventh language implementation. Here are the previous ones:

-   [parents](https://github.com/dhconnelly/parents): A bytecode VM and
    compiler for a dynamic language. Implemented in TypeScript. Manages its own
    heap and implements mark-and-sweep garbage collection rather than
    delegating to the JS runtime. Supports first-class functions with closure
    and uses this to implement lists and so on.
-   (incomplete) [june-old](https://github.com/dhconnelly/june-old): A bytecode
    VM and compiler for a Scheme variant. Implemented in C++. Only compiles and
    evaluates conditionals and assignment.
-   [ungulate](https://github.com/dhconnelly/ungulate): An interpreter for a
    Scheme variant, written in OCaml.
-   (incomplete) [crab](https://github.com/dhconnelly/crab): A bytecode VM and
    compiler for a simple dynamic language. Implemented in Rust. I never
    implemented garbage collection and assignment just copies entire values.
-   [yalig](https://github.com/dhconnelly/yalig): Another interpreter for a
    Scheme variant, written in Go.
-   [ts](https://github.com/dhconnelly/ts): An extremely simple interpreter
    implemented in C++.

I've read a lot of stuff that made this possible. Here are the resources that
helped me the most:

-   [Crafting Interpreters](https://craftinginterpreters.com/)
-   [Writing A Compiler In Go](https://compilerbook.com/)
-   [Writing An Interpreter In Go](https://interpreterbook.com/)
-   [My First Language Frontend with LLVM Tutorial](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html)

I've also read most of SICP and poked around in the Engineering Compilers book.

I also briefly worked on the GCL team at Google. (GCL is a bizarre internal
language used to generate protocol buffers in a whole bunch of different use
cases, including configuration of Borg jobs as well as configurations for live
experiments; there are some more details in [this
paper](https://research.tue.nl/en/studentTheses/gcl-viewer)). For various
reasons I didn't stay long enough to have any impact, but I was lucky to have
the opportunity to review a bunch of code written by the team for a new
implementation of the language, including some of the bytecode VM and compiler
as well as garbage collection. This is when I learned that garbage collection
can be viewed as trivial if you're just interested in the basic algorithm: the
engineer who implemented it told me he wrote the code in a single afternoon.
This was a good (if brief) learning experience.

## license

MIT
