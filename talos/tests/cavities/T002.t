Test that a cavity with a fixed condition is not detected as an unbounded
cavity.

HACK: We need to run in the repository with our source code in order for cabal
to work.  Cram normally changes to a tmp directory.

  $ cd $TESTDIR

Build Talos.

  $ cabal build exe:talos 2>&1 > /dev/null && echo "Build succeeded"
  Build succeeded

Run the test.

  $ cabal run exe:talos -- --find-cavities testdata/T002.ddl
  digraph Main {
    init -> 10;
    init [style = invis];
    18 [style = invis];
    10 [label = "x = Loop"];
    10 -> 11[label = "loop"];
    10 -> 12[label = "exit"];
    11 [label = "x = match1 { ... }"];
    11 -> 10;
    12 [label = "_x7 = x == "START""];
    12 -> 14;
    14 [label = "Case _x7"];
    14 -> 15[label = "True"];
    15 [label = "()"];
    15 -> 16;
    16 [label = "Loop"];
    16 -> 17[label = "loop"];
    16 -> 18[label = "exit"];
    17 [label = "match1 { ... }"];
    17 -> 16;
  }
  Cavity locations:
  Main:17
