This directory contains resources for packaging all the source code
needed to build a standalone `daedalus` executable.

To package up the code run the script `make-package` from the
root directory of the repo:

    > ./standalone/make-package

This needs to be done on a machine that has access to the source code
for the packages (for example, hackage.haskell.org).  The script will
build the entire `daedalus` project, and after than it will download
all packages that were used in the proces.  If everything completes
successfully, the directory `daedalus-standalone` should contain all
source code needed to build the `daedalus` executable.

    > ls daedalus-standalone/
    build          daedalus.cabal  daedalus-utils  daedalus-vm  rts-c   rts-vm-hs
    cabal.project  daedalus-core   daedalus-value  extern       rts-hs  src

Externally downloaded packages are in the `extern` sub-directory.

To build the `daedalus` executable on another machine:

  1. Copy `daedalus-standalone` to the target machine,
  2. Run the `./build` script,
  3. If successful, the `daedalus` executable will be generated in
     a subdirectory called `bin`.

The `build` script assumes that the following executables are installed on
the system:

  * `ghc`           (version 8.10.7), and
  * `cabal-install` (version 3.8.1.0)

Binaries for these tools may be downloaded from:

  * `ghc`: https://www.haskell.org/ghc/download_ghc_8_10_7.html#binaries
  * `cabal-install`: https://www.haskell.org/cabal/download.html




