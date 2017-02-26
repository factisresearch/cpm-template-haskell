Patched template-haskell code for the [checkpad project](http://cpmed.de)

The linker in Mac OSX Sierra has a hard limit on the size of load commands
in dynamically linked binaries. There is a
[ghc ticket](https://ghc.haskell.org/trac/ghc/ticket/12479) which claims to
fix the bug, but the [checkpad project](http://cpmed.de) still hits it.

Our workaround is:

* Use static linking.
* Only call functions from template haskell that are defined outside of the main project.
  This avoids dynamic linking of code from our project for executing
  template haskell code.

This repository contains functions invoked from template haskell. They
were once part of the main checkpad repository, but have now their own
repository, for reasons just explained.
