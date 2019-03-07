# vampire-proof-check

A simple tool to check proofs with vampire.
Inspired by a similar script by Bernhard Gleiss.


## How to build/install

1. Install the Haskell build tool `stack` from https://docs.haskellstack.org/en/stable/README/

2. `cd vampire-proof-check`

3. Run `stack build` to build the program. This should also download the compiler and all dependencies.

4. Run `stack install` to install the program to `~/.local/bin/`.
   Alternatively, the compiled program can be executed with `stack exec -- vampire-proof-check <args>`.


## Options

Run `vampire-proof-check --help` for a list of options.
