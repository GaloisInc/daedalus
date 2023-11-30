Test that `Many UInt8` is detected as a cavity.

HACK: We need to run in the repository with our source code in order for cabal
to work.  Cram normally changes to a tmp directory.

  $ cd $TESTDIR

Build Talos.

  $ cabal build exe:talos 2>&1 > /dev/null && echo "Build succeeded"
  Build succeeded

Run the test.

  $ cabal run exe:talos -- --find-cavities testdata/T024.ddl
  digraph Main {
    init -> 16;
    init [style = invis];
    25 [style = invis];
    16 [label = "a = match1 { 97 @(Word 8) .. 122 @(Word 8) }"];
    16 -> 18;
    18 [label = "r = Loop"];
    18 -> 20[label = "loop"];
    18 -> 24[label = "exit"];
    20 [label = "b = match1 { 97 @(Word 8) .. 122 @(Word 8) }"];
    20 -> 21;
    21 [label = "_x14 = b < a"];
    21 -> 22;
    22 [label = "Case _x14"];
    22 -> 23[label = "True"];
    23 [label = "r = ()"];
    23 -> 18;
    24 [label = "Main {r = r}"];
    24 -> 25;
  }
  No cavities.

XXX: False negative.  There should be a cavity, because an attacker can pick a
permissive value in the first match.  However, our expression analysis
currently considers inequalities across unconstrained values to be a
constraint.
