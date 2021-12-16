-- Referece: https://www.color.org/specification/ICC.2-2019.pdf
-- Reference for older features: https://www.color.org/icc32.pdf

import Debug
import Daedalus
import ICC

def Main =
  block
    let doc = ICC
    map (i in doc.tags) (ValidateTag i)

def ValidateTag (t : Tag) =
  case mpeInTag t of
    nothing  -> nothing
    just x   -> just (ValidateMPE x)

def ValidateMPE (mpe : MPElement) : calcInfo =
  case mpe.body of
    calc x        -> CheckFunOpsInCalc mpe.head x
    cvst          -> calcInfo mpe.head
    matf          -> calcInfo mpe.head
    mpet x        -> ValidateMPET mpe.head x
    unimplemented -> calcInfo mpe.head

def ValidateMPET h (els : [MPElement]) =
  block
    let f0 = Index els 0
    for (info = ValidateMPE f0; i,el in els)
      if i == 0
        then info
        else block
               let new = ValidateMPE el
               if info.outputs != new.inputs
                  then ErrorAt el.head.offset "input-output mismatch in mpet"
                  else Accept
               block
                  tag      = info.tag
                  inputs   = info.inputs
                  outputs  = new.outputs
                  stack    = 0
                  maxStack = max info.maxStack new.maxStack
                  cost     = info.cost + new.cost


def mpeInTag (t : Tag) =
  case t of
    desc -> nothing

    A2B0 x -> mpeInLutAB_or_multi x
    A2B1 x -> mpeInLutAB_or_multi x
    A2B2 x -> mpeInLutAB_or_multi x
    A2B3 x -> mpeInLutAB_or_multi x
    A2M0 x -> just x

    B2A0 x -> mpeInLutBA_or_multi x
    B2A1 x -> mpeInLutBA_or_multi x
    B2A2 x -> mpeInLutBA_or_multi x
    B2A3 x -> mpeInLutBA_or_multi x

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

    _      -> nothing

def mpeInLutAB_or_multi (x : LutAB_or_multi) =
  case x of
    lutAB -> nothing
    mpe y -> just y

def mpeInLutBA_or_multi (x : LutBA_or_multi) =
  case x of
    lutBA -> nothing
    mpe y -> just y







--------------------------------------------------------------------------------
-- Stack validation for FunOps


-- Initiial state of the stack
def calcInfo (h : MPElementHead) =
  block
    tag       = h.offset
    inputs    = h.inputs as int
    outputs   = h.outputs as int
    stack     = 0 : int
    maxStack  = 0 : int
    cost      = 0 : int

def addCost n (s : calcInfo) : calcInfo =
  block
    tag      = s.tag
    inputs   = s.inputs
    outputs  = s.outputs
    stack    = s.stack
    maxStack = s.maxStack
    cost     = s.cost + n

def ErrorAt loc msg =
  block
    SetStream loc
    Trace (concat [ "ERROR: ", msg ])

def FunOpError (op : FunOpWithPosition) msg = ErrorAt op.offset msg

def FunOpAssert (op : FunOpWithPosition) p msg =
  if p then Accept
       else FunOpError op msg

-- Push arguments on the stack
def funOpPush n (x : calcInfo) : calcInfo =
  block
    let newStack = x.stack + n
    tag      = x.tag
    inputs   = x.inputs
    outputs  = x.outputs
    stack    = newStack
    maxStack = if newStack > x.maxStack then newStack else x.maxStack
    cost     = x.cost

-- Remove some arguments from the stack
def FunOpPop op n (x : calcInfo) : calcInfo =
  block
    let newStack = if x.stack >= n
      then x.stack - n
      else block
             FunOpError op "Not enough arguments"
             0
    block
      tag      = x.tag
      inputs   = x.inputs
      outputs  = x.outputs
      stack    = newStack
      maxStack = x.maxStack
      cost     = x.cost

-- Specifies effect of an instruction on the stack.
-- input arguments are removed; output arguments are pushed
def FunOpArgs op inArgs outArgs calc =
  funOpPush outArgs (FunOpPop op inArgs calc)
--------------------------------------------------------------------------------


def CheckFunOpsInCalc h (c : CalcElement) =
  block
    let subs = map (mpe in c.subElements) (ValidateMPE mpe)
    SetStream h.offset
    Trace "Validating `calc`"
    $$ = CheckFunOps subs c c.main (calcInfo h)
    if $$.stack != 0
      then Trace "Stack is not empty at the end of calculation."
      else Accept

def arg1 (p : OpParams) = p.s as int
def arg2 (p : OpParams) = p.t as int
def theArg (p : OpParam)  = p as int

def DoCallSub op (sub : calcInfo) (s : calcInfo) : calcInfo =
  block
    let s1 = funOpPush sub.maxStack s
    let s2 = FunOpPop  op sub.maxStack s1
    let s3 = FunOpArgs op sub.inputs sub.outputs s2
    addCost sub.cost s3

def CallSub (c : CalcElement)
            (subs : [ calcInfo ])
            op
            (expect : uint 32)
            (n : uint 64)
            (s : calcInfo) =
  case Optional (Index c.subElements n) of
    nothing  ->
      block
        FunOpError op "sub element index out of bounds"
        s
    just mpe ->
      block
        FunOpAssert op (expect == 0 || mpe.head.tag == expect)
                                                "invalid sub-element type"
        DoCallSub op (Index subs n) s


def JoinInfo op x y : calcInfo =
  block
    FunOpAssert op (x.stack == y.stack) "Stack differs in alternatives"
    block
      tag       = x.tag
      inputs    = x.inputs
      outputs   = x.outputs
      stack     = x.stack
      maxStack  = max x.maxStack y.maxStack
      cost      = max x.cost     y.cost


def CheckFunOps (subs : [calcInfo])
                (c    : CalcElement)
                (ops  : [FunOpWithPosition])
                (startCalc : calcInfo) =
  for (calc = startCalc; op in ops)
    case op.op of
      data -> FunOpArgs op 0 1 (addCost 1 calc)

      opIn p  ->
        block
          let args = arg2 p + 1
          FunOpAssert op (arg1 p + args <= calc.inputs)
                                                     "Invalid channel in `in`"
          FunOpArgs op 0 args (addCost 1 calc)

      opOut p ->
        block
          let args  = arg2 p + 1
          FunOpAssert op (arg1 p + args <= calc.outputs)
                                                    "Invalid channel in `out`"
          FunOpArgs op args 0 (addCost 1 calc)

      opTGet p -> FunOpArgs op 0 (arg2 p + 1) (addCost 1 calc)
      opTPut p -> FunOpArgs op (arg2 p + 1) 0 (addCost 1 calc)

      opTSave p ->
        block
          let n = arg2 p + 1
          FunOpArgs op n n (addCost 1 calc)

      opEnv -> FunOpArgs op 0 2 (addCost 1 calc)

      curv n -> CallSub c subs op 0s"cvst" n calc
      mtx n  -> CallSub c subs op 0s"matf" n calc
      clut n -> CallSub c subs op 0s"sngf" n calc
      calc n -> CallSub c subs op 0s"calc" n calc
      tint n -> CallSub c subs op 0        n calc -- XXX: check type
      elem n -> CallSub c subs op 0        n calc

      copy p -> FunOpArgs op (arg1 p + 1) ((arg1 p + 1) * (arg2 p + 2))
                (addCost 1 calc)
      rotl p -> FunOpArgs op (arg1 p + 1) (arg2 p + 1)
                (addCost 1 calc)
      rotr p -> FunOpArgs op (arg1 p + 1) (arg2 p + 1)
                (addCost 1 calc)
      posd p -> FunOpArgs op (arg1 p + 1) ((arg1 p + 1) + (arg2 p + 1))
                (addCost 1 calc)

      -- BUG in spec
      flip s -> FunOpArgs op (theArg s + 2) (theArg s + 2)
                (addCost 1 calc)

      pop s  -> FunOpArgs op (theArg s+1) 0
                (addCost 1 calc)

      solv p -> FunOpArgs op ((arg1 p + 1) * (arg2 p + 2)) (arg2 p + 2)
                (addCost 1 calc)

      tran p ->
        block
          let els = (arg2 p + 1) * (arg1 p + 1)
          FunOpArgs op els els
                    (addCost 1 calc)

      sum n  -> FunOpArgs op (theArg n+2) 1 (addCost 1 calc)
      prod n -> FunOpArgs op (theArg n+2) 1 (addCost 1 calc)
      min n  -> FunOpArgs op (theArg n+2) 1 (addCost 1 calc)
      max n  -> FunOpArgs op (theArg n+2) 1 (addCost 1 calc)
      and n  -> FunOpArgs op (theArg n+2) 1 (addCost 1 calc)
      or n   -> FunOpArgs op (theArg n+2) 1 (addCost 1 calc)

      opPi      -> FunOpArgs op 0 1 (addCost 1 calc)
      opPosInf  -> FunOpArgs op 0 1 (addCost 1 calc)
      opNegInf  -> FunOpArgs op 0 1 (addCost 1 calc)
      opNaN     -> FunOpArgs op 0 1 (addCost 1 calc)

      opAdd s -> FunOpArgs op (2 * (theArg s+1)) (theArg s+1) (addCost 1 calc)
      opSub s -> FunOpArgs op (2 * (theArg s+1)) (theArg s+1) (addCost 1 calc)
      opMul s -> FunOpArgs op (2 * (theArg s+1)) (theArg s+1) (addCost 1 calc)
      opDiv s -> FunOpArgs op (2 * (theArg s+1)) (theArg s+1) (addCost 1 calc)
      opMod s -> FunOpArgs op (2 * (theArg s+1)) (theArg s+1) (addCost 1 calc)
      opPow s -> FunOpArgs op (2 * (theArg s+1)) (theArg s+1) (addCost 1 calc)

      opGamma s -> FunOpArgs op (theArg s + 2) (theArg s + 1) (addCost 1 calc)
      opSAdd s  -> FunOpArgs op (theArg s + 2) (theArg s + 1) (addCost 1 calc)
      opSSub s  -> FunOpArgs op (theArg s + 2) (theArg s + 1) (addCost 1 calc)
      opSMul s  -> FunOpArgs op (theArg s + 2) (theArg s + 1) (addCost 1 calc)
      opSDiv s  -> FunOpArgs op (theArg s + 2) (theArg s + 1) (addCost 1 calc)

      opSq   s  -> FunOpArgs op (theArg s + 1) (theArg s + 1) (addCost 1 calc)
      opSqrt s  -> FunOpArgs op (theArg s + 1) (theArg s + 1) (addCost 1 calc)
      opCb   s  -> FunOpArgs op (theArg s + 1) (theArg s + 1) (addCost 1 calc)
      opCbrt s  -> FunOpArgs op (theArg s + 1) (theArg s + 1) (addCost 1 calc)
      opAbs  s  -> FunOpArgs op (theArg s + 1) (theArg s + 1) (addCost 1 calc)
      opNeg  s  -> FunOpArgs op (theArg s + 1) (theArg s + 1) (addCost 1 calc)
      opRond s  -> FunOpArgs op (theArg s + 1) (theArg s + 1) (addCost 1 calc)
      opFlor s  -> FunOpArgs op (theArg s + 1) (theArg s + 1) (addCost 1 calc)
      opCeil s  -> FunOpArgs op (theArg s + 1) (theArg s + 1) (addCost 1 calc)
      opTrnc s  -> FunOpArgs op (theArg s + 1) (theArg s + 1) (addCost 1 calc)
      opSign s  -> FunOpArgs op (theArg s + 1) (theArg s + 1) (addCost 1 calc)
      opExp  s  -> FunOpArgs op (theArg s + 1) (theArg s + 1) (addCost 1 calc)
      opLog  s  -> FunOpArgs op (theArg s + 1) (theArg s + 1) (addCost 1 calc)
      opLn   s  -> FunOpArgs op (theArg s + 1) (theArg s + 1) (addCost 1 calc)
      opSin  s  -> FunOpArgs op (theArg s + 1) (theArg s + 1) (addCost 1 calc)
      opCos  s  -> FunOpArgs op (theArg s + 1) (theArg s + 1) (addCost 1 calc)
      opTan  s  -> FunOpArgs op (theArg s + 1) (theArg s + 1) (addCost 1 calc)
      opASin s  -> FunOpArgs op (theArg s + 1) (theArg s + 1) (addCost 1 calc)
      opACos s  -> FunOpArgs op (theArg s + 1) (theArg s + 1) (addCost 1 calc)
      opATan s  -> FunOpArgs op (theArg s + 1) (theArg s + 1) (addCost 1 calc)
      opATn2 s  -> FunOpArgs op (theArg s + 1) (theArg s + 1) (addCost 1 calc)

      opCTop s  -> FunOpArgs op (2 * (theArg s + 1)) (2 * (theArg s + 1))
                   (addCost 1 calc)
      opPToc s  -> FunOpArgs op (2 * (theArg s + 1)) (2 * (theArg s + 1))
                   (addCost 1 calc)
      opRNum s  -> FunOpArgs op (theArg s + 1) (theArg s + 1)
                   (addCost 1 calc)

      opLT s    -> FunOpArgs op (2 * (theArg s+1)) (theArg s+1) (addCost 1 calc)
      opLE s    -> FunOpArgs op (2 * (theArg s+1)) (theArg s+1) (addCost 1 calc)
      opEQ s    -> FunOpArgs op (2 * (theArg s+1)) (theArg s+1) (addCost 1 calc)
      opNear s  -> FunOpArgs op (2 * (theArg s+1)) (theArg s+1) (addCost 1 calc)
      opGE s    -> FunOpArgs op (2 * (theArg s+1)) (theArg s+1) (addCost 1 calc)
      opGT s    -> FunOpArgs op (2 * (theArg s+1)) (theArg s+1) (addCost 1 calc)
      opVMin s  -> FunOpArgs op (2 * (theArg s+1)) (theArg s+1) (addCost 1 calc)
      opVMax s  -> FunOpArgs op (2 * (theArg s+1)) (theArg s+1) (addCost 1 calc)
      opVAnd s  -> FunOpArgs op (2 * (theArg s+1)) (theArg s+1) (addCost 1 calc)
      opVOr s   -> FunOpArgs op (2 * (theArg s+1)) (theArg s+1) (addCost 1 calc)

      opTLab s  -> FunOpArgs op (3 * (theArg s+1)) (3 * (theArg s+1))
                  (addCost 1 calc)

      opTXYZ s  -> FunOpArgs op (3 * (theArg s+1)) (3 * (theArg s+1))
                  (addCost 1 calc)


      opIfThen thenOps ->
        block
          let calc1 = FunOpPop op 1 calc
          let calc2 = CheckFunOps subs c thenOps calc1
          addCost 1 (JoinInfo op calc1 calc2)

      opIfThenElse ops ->
        block
          let calc1 = FunOpPop op 1 calc
          let calc2 = CheckFunOps subs c ops.thenOps calc1
          let calc3 = CheckFunOps subs c ops.elseOps calc1
          addCost 1 (JoinInfo op calc2 calc3)

      opSel ops ->
        block
          let calc1   = FunOpPop op 1 calc
          let res     = addCost 1 (CheckFunOps subs c ops.case1 calc1)
          let resAlts = for (eff = res; alt in ops.cases)
                            (JoinInfo op eff (CheckFunOps subs c alt eff))
          case ops.dflt of
            nothing -> JoinInfo op calc1 resAlts
            just x  -> JoinInfo op (CheckFunOps subs c x calc1) resAlts

