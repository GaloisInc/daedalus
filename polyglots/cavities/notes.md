# Notes

Notes on possible analyses.

## Data flow + Control dependency

Goal. Determine whether a byte read from the stream is unconstrained or
semi-constrained.

Approach. A byte is unconstrained if it does not influence a control point that
leads to parser failure.  It's semi-constrained if it does influence one or
more control points, but the predicates are negative.

A read influences a control point if there is a path from the read to the
control predicate in the data dependence graph.  A control point leads to
parser failure if there is a transitive path in the control dependence graph.

### Examples

```
-- A -> B
--   <-
-- A -> B
--   <-
-- A
def Main = 
  block
    let a = UInt8
    F a
    let b = UInt8
    F b
    (a, b)

def F x =
  block
    x < 255 is True
    x
```
