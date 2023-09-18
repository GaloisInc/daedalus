Test that a recursive loop is detected as a cavity.

HACK: We need to run in the repository with our source code in order for cabal
to work.  Cram normally changes to a tmp directory.

  $ cd $TESTDIR

Build Talos.

  $ cabal build exe:talos 2>&1 > /dev/null && echo "Build succeeded"
  Build succeeded

Run the test.  The test is named "broken" because it contains END, which Talos
does not support for synthesis.

  $ cabal run exe:talos -- --find-cavities testdata/T003.broken.ddl
  digraph RecLoop {
    init -> 22;
    init [style = invis];
    36 [style = invis];
    22 [label = "Or (biased) "];
    22 -> 24;
    22 -> 27;
    24 [label = "matchEnd"];
    24 -> 25;
    25 [label = "bldr"];
    25 -> 36;
    27 [label = "c = match1 { ... }"];
    27 -> 28;
    28 [label = "_aRecLoop_0 = emit bldr c"];
    28 -> 29;
    29 [label = "RecLoop_aRecLoop_0"];
    29 -> 36;
  }
  digraph Main {
    init -> 31;
    init [style = invis];
    37 [style = invis];
    31 [label = "match_ "START""];
    31 -> 32;
    32 [label = "_aRecLoop_0 = nil @(Word 8)"];
    32 -> 34;
    34 [label = "_7 = RecLoop_aRecLoop_0"];
    34 -> 35;
    35 [label = "listToArray _7"];
    35 -> 37;
  }
  Cavity locations:
  RecLoop:27 embedded
