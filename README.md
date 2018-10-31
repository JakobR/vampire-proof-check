# vampire-proof-checker

A simple tool to check proofs with vampire.
Inspired by a similar script by Bernhard Gleiss.


## How to build

* Install the Haskell build tool `stack` from https://docs.haskellstack.org/en/stable/README/

* Run `stack build` to build the program. This should also download the compiler and all dependencies.

* Run the compiled program with `stack exec vampire-proof-checker -- <args>`.
