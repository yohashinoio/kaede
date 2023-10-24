# Installation

## Install dependencies with homebrew

### LLVM 15

```bash
$ brew install llvm@15
```

### Environment Variable Configuration

```bash
$ export LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix)/lib"
```

If you want to make it permanent, put it in a configuration file of a shell you are using.
Below is for bash.

```bash
$ echo 'export LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix)/lib"' >> ~/.bashrc
```

# Why write the boot compiler in Rust?

When I began development, I did not expect development to continue through to the self-hosting phase.

Therefore, I decided to develop in Rust to make the compiler as fast as possible.

Also the reason for choosing Rust over C++ was simple: I had written a compiler in C++ before.

Ever since I remembered that it might be possible to self-host, I regret that I should have written it in OCaml or something like that instead of Rust.

# License

This project is available under the dual Apache 2.0 and MIT license.<br/>
See LICENSE-* for the full content of the licenses.