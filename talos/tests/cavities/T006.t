Test that a cavity with a fixed condition is not detected as an unbounded
cavity.

HACK: We need to run in the repository with our source code in order for cabal
to work.  Cram normally changes to a tmp directory.

  $ cd $TESTDIR

Build Talos.

  $ cabal build exe:talos 2>&1 > /dev/null && echo "Build succeeded"
  Build succeeded

Run the test.

  $ cabal run exe:talos -- --find-cavities testdata/T006.ddl
  digraph UpTo_27 {
    init -> 40;
    init [style = invis];
    61 [style = invis];
    40 [label = "Or (biased) "];
    40 -> 41;
    40 -> 48;
    41 [label = "_x39 = "\n""];
    41 -> 43;
    43 [label = "match_ _x39"];
    43 -> 45;
    45 [label = "_x39"];
    45 -> 46;
    46 [label = "bldr"];
    46 -> 61;
    48 [label = "_7 = match1 { ... }"];
    48 -> 49;
    49 [label = "_8 = emit bldr _7"];
    49 -> 50;
    50 [label = "UpTo_27_8"];
    50 -> 61;
  }
  digraph Main {
    init -> 51;
    init [style = invis];
    62 [style = invis];
    51 [label = "_aUpTo_27_0 = nil @(Word 8)"];
    51 -> 53;
    53 [label = "_9 = UpTo_27_aUpTo_27_0"];
    53 -> 54;
    54 [label = "x = listToArray _9"];
    54 -> 55;
    55 [label = "_x36 = x == "START""];
    55 -> 57;
    57 [label = "Case _x36"];
    57 -> 58[label = "True"];
    58 [label = "()"];
    58 -> 59;
    59 [label = "Loop"];
    59 -> 60[label = "loop"];
    59 -> 62[label = "exit"];
    60 [label = "match1 { ... }"];
    60 -> 59;
  }
  Cavity locations:
  UpTo_27:48 (prefix)
  Main:60 (suffix)

XXX: There should not be a cavity detected at UpTo_27:48.  This is a false
positive caused by path insensitivity.  Consider these two paths:

1. P immediately matches in the first call to UpTo.  The cavity is never
visited, hence the cavity is not in the bounded set at program exit.

2. P does not match, and we read a UInt8 and recurse into UpTo.  On the next
iteration, this stream read is entered into its read frontier.

Together, these paths indicate a cavity.  However, path 1 is infeasible;
x cannot be "START". 

