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

# License

This project is available under the dual Apache 2.0 and MIT license.<br/>
See LICENSE-* for the full content of the licenses.