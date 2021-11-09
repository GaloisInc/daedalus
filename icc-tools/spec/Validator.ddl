-- Referece: https://www.color.org/specification/ICC.2-2019.pdf
-- Reference for older features: https://www.color.org/icc32.pdf

import Daedalus
import ICC

{-
def mpeInTag (t : Tag) =
  case t of
    desc -> nothing

    -- A2B0 x -> mpeInLutAB_or_multi x
    -- A2B1 x -> mpeInLutAB_or_multi x
    -- A2B2 x -> mpeInLutAB_or_multi x
    -- A2B3 x -> mpeInLutAB_or_multi x
    A2M0 x -> just x

    -- B2A0 x -> mpeInLutBA_or_multi x
    -- B2A1 x -> mpeInLutBA_or_multi x
    -- B2A2 x -> mpeInLutBA_or_multi x
    -- B2A3 x -> mpeInLutBA_or_multi x

    B2D0 x -> just x
    B2D1 x -> just x
    B2D2 x -> just x
    B2D3 x -> just x

    wtpt   -> nothing
    cprt   -> nothing
    c2sp x -> just x
    s2cp x -> just x

    svcn   -> nothing

    rXYZ   -> nothing
    gXYZ   -> nothing
    bXYZ   -> nothing

    rTRC   -> nothing
    gTRC   -> nothing
    bTRC   -> nothing

    dmdd   -> nothing
    dmnd   -> nothing

    chrm   -> nothing
    chad   -> nothing
-}
def mpeInLutAB_or_multi (x : LutAB_or_multi) =
  case x of
    lutAB -> nothing
    mpe y -> just y

{-
def mpeInLutBA_or_multi (x : LutBA_or_multi) =
  case x of
    lutBA -> nothing
    mpe y -> just y
-}



{-
def Main =
  block
    let doc = ICC
    -- map (i in doc.tags) (ValidateTag i)


    _       -> Accept
-}

--------------------------------------------------------------------------------
-- Stack validation for FunOps


-- Initiial state of the stack
def funOpChecker = { stack = 0 : uint 64 }

-- Push arguments on the stack
def funOpPush n (x : funOpChecker) : funOpChecker =
  { stack = x.stack + n }

-- Remove some arguments from the stack
def FunOpPop op n (x : funOpChecker) : funOpChecker =
  block
    FunOpAssert op (x.stack >= n) "Not enough arguments for operation"
    { stack = x.stack - n }

-- Get the size of the stack
def funOpStackSize (x : funOpChecker) = x.stack

-- Check a certain condition and report an error if the condition fails
def FunOpAssert op b msg =
  First
    Guard b
    block
      SetStream op.offset
      Fail msg

-- Specifies effect of an instruction on the stack.
-- input arguments are removed; output arguments are pushed
def FunOpArgs op inArgs outArgs calc =
  funOpPush outArgs (FunOpPop op inArgs calc)
--------------------------------------------------------------------------------


def CheckFunOpsInCalc (c : CalcElement) =
  Guard (CheckFunOps c c.main funOpChecker == funOpChecker)
    <| Fail "Left over elements on the stack"

def CheckFunOps (c : CalcElement) (ops  : [FunOpWithPosition]) startCalc =
  for (calc = startCalc; op in ops)
    block
    SetStream op.offset
    case op.op of
      data    -> funOpPush 1 calc

      opIn p  ->
        block
          let n = p.t + 1
          FunOpAssert op (p.s < c.inputs && n <= (c.inputs - p.s) )
                                                     "Invalid channel in `in`"
          FunOpArgs op 0 n calc

      opOut p ->
        block
          let n = p.t + 1
          FunOpAssert op (p.s < c.outputs && n <= (c.outputs - p.s))
                                                    "Invalid channel in `out`"

          FunOpArgs op n 0 calc

      opTGet p -> FunOpArgs op 0 (p.t + 1) calc
      opTPut p -> FunOpArgs op (p.t + 1) 0 calc

      opTSave p ->
        block
          let n = p.t + 1
          FunOpArgs op n n calc

      opEnv -> FunOpArgs op 0 2 calc

      curv n ->
        block
          let mpe = Index c.subElements n
                    <| Fail "`curv` sub element index out of bounds"
          mpe.body is cvst <| Fail "`curv` argument is not a curve"
          let h = mpe.head
          FunOpArgs op h.inputs h.outputs calc

      mtx n ->
        block
          let mpe = Index c.subElements n
                    <| Fail "`mtx` sub element index out of bounds"
          mpe.body is matf <| Fail "`mtx` sub element is not a matrix"
          let h = mpe.head
          FunOpArgs op h.inputs h.outputs calc

      clut n ->
        block
          let mpe = Index c.subElements n
                    <| Fail "`clut` sub element index out of bounds"
          -- mpe.body is matf   XXX: CHECK TYPE
          let h = mpe.head
          FunOpArgs op h.inputs h.outputs calc

      calc n ->
        block
          let mpe = Index c.subElements n
                    <| Fail "`calc` sub element index out of bounds"
          mpe.body is calc <| Fail "`calc` arguemtn is not a calculator"
          let h = mpe.head
          FunOpArgs op h.inputs h.outputs calc

      tint n ->
        block
          let mpe = Index c.subElements n
                    <| Fail "`tint` sub element index out of bounds"
          -- mpe.body is matf   XXX: CHECK TYPE
          let h = mpe.head
          FunOpArgs op h.inputs h.outputs calc

      elem n ->
        block
          let mpe = Index c.subElements n
                    <| Fail "`elem` sub element index out of bounds"
          let h = mpe.head
          FunOpArgs op h.inputs h.outputs calc

      copy p -> FunOpArgs op (p.s + 1) ((p.s + 1) * (p.t + 2)) calc
      rotl p -> FunOpArgs op (p.s + 1) (p.s + 1) calc
      rotr p -> FunOpArgs op (p.s + 1) (p.s + 1) calc
      posd p -> FunOpArgs op (p.s + 1) ((p.s + 1) + (p.t + 1)) calc

      -- BUG in spec
      flip s -> FunOpArgs op (s + 2) (s + 2) calc

      pop s  -> FunOpArgs op (s+1) 0 calc
      solv p -> FunOpArgs op ((p.s + 1) * (p.t + 2)) (p.t + 2) calc

      tran p ->
        block
          let els = (p.t + 1) * (p.s + 1)
          FunOpArgs op els els calc

      sum n  -> FunOpArgs op (n+2) 1 calc
      prod n -> FunOpArgs op (n+2) 1 calc
      min n  -> FunOpArgs op (n+2) 1 calc
      max n  -> FunOpArgs op (n+2) 1 calc
      and n  -> FunOpArgs op (n+2) 1 calc
      or n   -> FunOpArgs op (n+2) 1 calc

      opPi      -> FunOpArgs op 0 1 calc
      opPosInf  -> FunOpArgs op 0 1 calc
      opNegInf  -> FunOpArgs op 0 1 calc
      opNaN     -> FunOpArgs op 0 1 calc

      opAdd s -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opSub s -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opMul s -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opDiv s -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opMod s -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opPow s -> FunOpArgs op (2 * (s+1)) (s+1) calc

      opGamma s -> FunOpArgs op (s + 2) (s + 1) calc
      opSAdd s  -> FunOpArgs op (s + 2) (s + 1) calc
      opSSub s  -> FunOpArgs op (s + 2) (s + 1) calc
      opSMul s  -> FunOpArgs op (s + 2) (s + 1) calc
      opSDiv s  -> FunOpArgs op (s + 2) (s + 1) calc

      opSq   s  -> FunOpArgs op (s + 1) (s + 1) calc
      opSqrt s  -> FunOpArgs op (s + 1) (s + 1) calc
      opCb   s  -> FunOpArgs op (s + 1) (s + 1) calc
      opCbrt s  -> FunOpArgs op (s + 1) (s + 1) calc
      opAbs  s  -> FunOpArgs op (s + 1) (s + 1) calc
      opNeg  s  -> FunOpArgs op (s + 1) (s + 1) calc
      opRond s  -> FunOpArgs op (s + 1) (s + 1) calc
      opFlor s  -> FunOpArgs op (s + 1) (s + 1) calc
      opCeil s  -> FunOpArgs op (s + 1) (s + 1) calc
      opTrnc s  -> FunOpArgs op (s + 1) (s + 1) calc
      opSign s  -> FunOpArgs op (s + 1) (s + 1) calc
      opExp  s  -> FunOpArgs op (s + 1) (s + 1) calc
      opLog  s  -> FunOpArgs op (s + 1) (s + 1) calc
      opLn   s  -> FunOpArgs op (s + 1) (s + 1) calc
      opSin  s  -> FunOpArgs op (s + 1) (s + 1) calc
      opCos  s  -> FunOpArgs op (s + 1) (s + 1) calc
      opTan  s  -> FunOpArgs op (s + 1) (s + 1) calc
      opASin s  -> FunOpArgs op (s + 1) (s + 1) calc
      opACos s  -> FunOpArgs op (s + 1) (s + 1) calc
      opATan s  -> FunOpArgs op (s + 1) (s + 1) calc
      opATn2 s  -> FunOpArgs op (s + 1) (s + 1) calc

      opCTop s  -> FunOpArgs op (2 * (s + 1)) (2 * (s + 1)) calc
      opPToc s  -> FunOpArgs op (2 * (s + 1)) (2 * (s + 1)) calc
      opRNum s  -> FunOpArgs op (s + 1) (s + 1)             calc

      opLT s    -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opLE s    -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opEQ s    -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opNear s  -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opGE s    -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opGT s    -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opVMin s  -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opVMax s  -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opVAnd s  -> FunOpArgs op (2 * (s+1)) (s+1) calc
      opVOr s   -> FunOpArgs op (2 * (s+1)) (s+1) calc

      opTLab s  -> FunOpArgs op (3 * (s+1)) (3 * (s+1)) calc
      opTXYZ s  -> FunOpArgs op (3 * (s+1)) (3 * (s+1)) calc

      opIfThen thenOps ->
        block
          let calc1 = FunOpPop op 1 calc
          let calc2 = CheckFunOps c thenOps calc1
          FunOpAssert op (calc1 == calc2)
                        "`if-then` does not preserve the stack size"
          calc1

      opIfThenElse ops ->
        block
          let calc1 = FunOpPop op 1 calc
          let calc2 = CheckFunOps c ops.thenOps calc1
          let calc3 = CheckFunOps c ops.elseOps calc1
          FunOpAssert op (calc2 == calc3)
                        "`if-then-else` branches affect stack differently"
          calc2

      opSel ops ->
        block
          let calc1 = FunOpPop op 1 calc
          let res = CheckFunOps c ops.case1 calc1
          map (alt in ops.cases)
            (FunOpAssert op (CheckFunOps c alt calc1 == res)
                "`cases` in `sel` affect stack differently")
          case ops.dflt of
            nothing -> FunOpAssert op (res == calc1)
                        "`cases` in `sel` with no default do not preserve stack"
            just x  -> FunOpAssert op (CheckFunOps c x calc1 == res)
                         "`dflt` in `case` has different effect from `case`s"
          res


