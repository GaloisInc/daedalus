Test that a recursive loop with builder and a guard is detected as a cavity.

HACK: We need to run in the repository with our source code in order for cabal
to work.  Cram normally changes to a tmp directory.

  $ cd $TESTDIR

Build Talos.

  $ cabal build exe:talos 2>&1 > /dev/null && echo "Build succeeded"
  Build succeeded

Run the test.

  $ cabal run exe:talos -- --find-cavities testdata/T005.ddl
  digraph RecLoop {
    init -> 19;
    init [style = invis];
    30 [style = invis];
    19 [label = "Or (biased) "];
    19 -> 20;
    19 -> 25;
    20 [label = "_x18 = "END""];
    20 -> 22;
    22 [label = "match_ _x18"];
    22 -> 23;
    23 [label = "_x18"];
    23 -> 30;
    25 [label = "c = match1 { ... }"];
    25 -> 26;
    26 [label = "_aRecLoop_0 = emit bldr c"];
    26 -> 27;
    27 [label = "RecLoop_aRecLoop_0"];
    27 -> 30;
  }
  digraph Main {
    init -> 28;
    init [style = invis];
    31 [style = invis];
    28 [label = "_aRecLoop_0 = nil @(Word 8)"];
    28 -> 29;
    29 [label = "RecLoop_aRecLoop_0"];
    29 -> 31;
  }
  Cavity locations:
  RecLoop:25 (prefix)
