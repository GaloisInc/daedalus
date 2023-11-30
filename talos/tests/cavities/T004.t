Test that a recursive loop with a guard is detected as a cavity.

HACK: We need to run in the repository with our source code in order for cabal
to work.  Cram normally changes to a tmp directory.

  $ cd $TESTDIR

Build Talos.

  $ cabal build exe:talos 2>&1 > /dev/null && echo "Build succeeded"
  Build succeeded

Run the test.

  $ cabal run exe:talos -- --find-cavities testdata/T004.ddl
  digraph RecLoop {
    init -> 10;
    init [style = invis];
    18 [style = invis];
    10 [label = "Or (biased) "];
    10 -> 12;
    10 -> 15;
    12 [label = "match_ "END""];
    12 -> 13;
    13 [label = """"];
    13 -> 18;
    15 [label = "match1_ { ... }"];
    15 -> 16;
    16 [label = "RecLoop"];
    16 -> 18;
  }
  digraph Main {
    init -> 17;
    init [style = invis];
    19 [style = invis];
    17 [label = "RecLoop"];
    17 -> 19;
  }
  Cavity locations:
  RecLoop:15 (prefix)
