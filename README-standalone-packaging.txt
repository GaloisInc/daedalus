This file contains instructions on how to build setup DaeDaLus so that
it can be built in an environment with no access to the internet.

1. Determine External Dependencies
==================================

These steps should be done in a normal development environment with
access to the internet.

    # Build the Daedalus executable
    cabal build --disable-tests --disable-benchmarks exe:daedalus

    # Build the cabal-pack executable
    cabal build --disable-tests --disable-benchmarks exe:cabal-pack

The program `cabal-pack` can examine the DaeDaLus build database and report
the external packages that were used during the build process.
The see the packages:

    # List the external packages in the Daedalus build
    cabal exec cabal-pack -- exe:daedalus

2. Download External Dependencies
=================================

The easiest way to download the external dependencies is to use `cabal-pack`.
Instead of listing the dependencies it can download them and unpack them
in a specified directory:

    # Download external dependencies to the `external` directory
    cabal exec cabal-pack -- exe:daedalus --download=external

If this is successful, then the directory `external` should contain a
number of sub-directories containing the source code for the external
packages that we use.


3. Packaging the Source Code
============================

At this point, the current directory should contain all the source code
required to build the `daedalus` binary.  Directories called `dist-newstyle`
contain build artifacts and may be deleted before packaging the source code.


4. Building DaeDaLus from the Sources
=====================================

These steps may be performed in a build environment, that does
not require external access.  We assume that build environment has
`ghc-8.10.7` and `cabal-3.8.1.0` already installed.

    # Create a file named `cabal.project.local` with this content:
    # This file should be located in the root directory of the sources
    # where there already is a file called `cabal.project`

    packages:
      external/*/*.cabal

This configuration file tells the build system to use packages in
the `external` directory before attempting to download them.

    # Build the `daedalus` executable
    cabal build --disable-tests --disable-benchmarks exe:daedalus

    # Copy the built executable to a location of your choice
    cp $(cabal exec which daedalus) /usr/local/bin



