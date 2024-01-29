> [!WARNING]
> As this project is still in the pre-release phase, there is still a possibility that the language specifications could change significantly!

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

If you want to make it permanent, put it in a shell initialization file.<br/>
For example, in the case of a `~/.profile`.

```bash
$ echo 'export LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix)/lib"' >> ~/.profile
```

## Install standard library and compiler

Make sure you have Python3 installed.

```bash
$ ./install.py
```

You can check if the installation was successful with the following command.

```bash
$ kaede --help
```

If it does not exist, please re-login once.

# Why write the boot compiler in Rust?

When I began development, I did not expect development to continue through to the self-hosting phase.

Therefore, I decided to develop in Rust to make the compiler as fast as possible.

Also, I chose Rust over C++ because I had previously written a compiler in C++ and was troubled by access to null pointers, template errors, and lack of a package manager (conan and others were available but underpowered).

Ever since I remembered that it might be possible to self-host, I regret that I should have written it in OCaml or something like that instead of Rust.

# License

This project is available under the dual Apache 2.0 and MIT license.<br/>
See LICENSE-\* for the full content of the licenses.
