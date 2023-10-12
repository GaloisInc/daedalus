Test that a recursive loop is detected as a cavity.

HACK: We need to run in the repository with our source code in order for cabal
to work.  Cram normally changes to a tmp directory.

  $ cd $TESTDIR

Build Talos.

  $ cabal build exe:talos 2>&1 > /dev/null && echo "Build succeeded"
  Build succeeded

Run the test.

  $ cabal run exe:talos -- --find-cavities testdata/T003.ddl
  digraph RecLoop {
    init -> 8;
    init [style = invis];
    10 [style = invis];
    8 [label = "match1 { ... }"];
    8 -> 10;
  }
  digraph Main {
    init -> 9;
    init [style = invis];
    11 [style = invis];
    9 [label = "RecLoop"];
    9 -> 11;
  }
  Cavity locations:
  RecLoop:8
