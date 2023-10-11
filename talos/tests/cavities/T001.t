Test that `Many UInt8` is detected as a cavity.

HACK: We need to run in the repository with our source code in order for cabal
to work.  Cram normally changes to a tmp directory.

  $ cd $TESTDIR

Build Talos.

  $ cabal build exe:talos 2>&1 > /dev/null && echo "Build succeeded"
  Build succeeded

Run the test.

  $ cabal run exe:talos -- --find-cavities testdata/T001.ddl
  digraph Main {
    init -> 5;
    init [style = invis];
    7 [style = invis];
    5 [label = "Loop"];
    5 -> 6[label = "loop"];
    5 -> 7[label = "exit"];
    6 [label = "match1 { ... }"];
    6 -> 5;
  }
  Cavity locations:
  Main:6
