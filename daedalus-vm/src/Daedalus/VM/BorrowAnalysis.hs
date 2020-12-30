{-# Language OverloadedStrings #-}
module Daedalus.VM.BorrowAnalysis(doBorrowAnalysis,modeI,modePrimName) where

import           Data.Maybe(catMaybes)
import           Data.Map(Map)
import qualified Data.Map as Map
import           Data.Set(Set)
import qualified Data.Set as Set

import Daedalus.Panic(panic)
import Daedalus.PP hiding (block)
import Daedalus.Core(FName,Op1(..),Op2(..),Op3(..),OpN(..))
import Daedalus.VM

{-
* Notes on reference variables:

  1. Each variable in a function/primitive can be:
    - "owned" which means the function should deallocate it when finished
    - "borrowed" which means the the variable:
      - is guaranteed to be alive for the duration of the call (but not longer!)
      - should not be deallocated by the function.

  2. A local variable is "owned"

  3. We can get an "owned" version of any variable ("borrowed" or "owned")
     by copying it.

  4. Many primtives can be passed arguments in either mode, and will adjust
     as neccessary (i.e, each primitive is a family of functions
     indexed by the ownership of its arguments).

  5. Basic blocks have a fixed modality on their argument and
     must be passed arguments in the expected way.

  6. We can only pass a "borrowed" variable as "borrowed"

  7. We can pass a variable we "own" as a "borrowed" argument, as long as
     we plan to come back from the call (i.e., NOT in a tail-call), so that
     we can deallocate the variable.

  8. We can pass a variable we "own" as an "owned" argument to another function
     but then we give up ownership and should not use this variable anymore.

  9. Variables that are alive after a non-tail call (i.e., they are in
     the stack allocated closure) can always be passed as "borrowed" to the
     function

* The purpose of this module is to compute the ownerships for the arguments
of basic blocks (point 5)

* (7) provides a HARD requirement we must satisfy:
  - we CANNOT jump to a BB expecting a "borrowed" argument using an
    "owned" variable.
  - we CAN jump to a BB expecting an "owned" argument using a "borrowed"
    variable by copying the variable first.

* We'd like to minimize copying, to do so we prefer to:
  - pass a "borrowed" argument as "borrowed"
  - pass an "owned" argument that we still need after the call as "borrowed"
  - pass an "owned" argument that we don't use after the call as "owned"
-}



doBorrowAnalysis :: Program -> Program
doBorrowAnalysis prog = prog { pBoot    = annBlock  <$> pBoot prog
                             , pModules = annModule <$> pModules prog
                             }
  where
  info        = borrowAnalysis prog
  annModule m = m { mFuns     = annFun   <$> mFuns m }
  annFun f    = f { vmfBlocks = annBlock <$> vmfBlocks f }
  annBlock b =
    case Map.lookup (blockName b) info of
      Just sig ->
        let as = zipWith annArg (blockArgs b) sig
            mp = Map.fromList [ (x,a) | a@(BA x _ _) <- as ]
        in b { blockArgs = as
             , blockInstrs = map (annI mp) (blockInstrs b)
             , blockTerm   = annTerm mp (blockTerm b)
             }

      Nothing  -> panic "doBorrowAnalysis"
                    [ "Missing ownership information for"
                    , show (pp (blockName b))
                    ]

  annArg (BA x t _) own = BA x t own

  annI mp i =
    case i of
      SetInput e      -> SetInput (annE mp e)
      Say {}          -> i
      Output e        -> Output (annE mp e)
      Notify e        -> Notify (annE mp e)
      CallPrim x p es -> CallPrim x p (map (annE mp) es)
      GetInput {}     -> i
      Spawn x l       -> Spawn x (annJ mp l)
      NoteFail        -> i
      Let x e         -> Let x (annE mp e)
      Free xs         -> Free (Set.map (annV mp) xs)

  annTerm mp t =
    case t of
      Jump l             -> Jump (annJ mp l)
      JumpIf e ls        -> JumpIf (annE mp e) (annJ2 mp ls)
      Yield              -> Yield
      ReturnNo           -> ReturnNo
      ReturnYes e        -> ReturnYes (annE mp e)
      ReturnPure e       -> ReturnPure (annE mp e)
      CallPure f l es    -> CallPure f (annJ mp l) (annE mp <$> es)
      Call f c no yes es -> Call f c (annJ mp no) (annJ mp yes) (annE mp <$> es)
      TailCall f c es    -> TailCall f c (annE mp <$> es)

  annVA mp v@(BA x _ _) = Map.findWithDefault v x mp

  annV mp v =
    case v of
      ArgVar a -> ArgVar (annVA mp a)
      LocalVar {} -> v

  annJ mp (JumpPoint l es) = JumpPoint l (map (annE mp) es)
  annJF mp jf = JumpWithFree { freeFirst = Set.map (annV mp) (freeFirst jf)
                             , jumpTarget = annJ mp (jumpTarget jf)
                             }
  annJ2 mp (JumpCase opts) = JumpCase (annJF mp <$> opts)

  annE mp e =
    case e of
      EBlockArg ba -> EBlockArg (annVA mp ba)
      _            -> e






--------------------------------------------------------------------------------
data Info = Info
  { iBlockOwned :: Set BA
  , iBlockInfo  :: Map Label [Ownership]
  , iFunEntry   :: Map FName Label
  , iChanges    :: Bool
  }

addBlockArg :: BA -> Ownership -> Info -> Info
addBlockArg x m i =
  case m of
    Owned | not (x `Set.member` iBlockOwned i) ->
             i { iChanges = True, iBlockOwned = Set.insert x (iBlockOwned i) }
    _ -> i

getBlockOwnership :: Label -> Info -> [Ownership]
getBlockOwnership l i = Map.findWithDefault (repeat Borrowed) l (iBlockInfo i)

getFunOwnership :: FName -> Info -> [Ownership]
getFunOwnership f i =
  case Map.lookup f (iFunEntry i) of
    Just l  -> getBlockOwnership l i
    Nothing -> panic "getFunOwnership" [ "Missing entry point for " ++ show (pp f) ]

--------------------------------------------------------------------------------


borrowAnalysis :: Program -> Map Label [Ownership]
borrowAnalysis p = loop i0
  where
  i0 = Info { iBlockOwned = Set.empty
            , iBlockInfo  = Map.empty
            , iChanges    = False
            , iFunEntry   = Map.fromList [ (vmfName f, vmfEntry f)
                                         | m <- pModules p, f <- mFuns m
                                         ]
            }

  loop i = let i1 = vmProgram p i
           in if iChanges i1
                then loop i1 { iChanges = False }
                else iBlockInfo i


vmProgram :: Program -> Info -> Info
vmProgram p i = foldr vmModule bs (pModules p)
  where
  bs = foldr block i (Map.elems (pBoot p))

vmModule :: Module -> Info -> Info
vmModule = foldr (.) id . map vmFun . mFuns

vmFun :: VMFun -> Info -> Info
vmFun = foldr (.) id . map block . Map.elems . vmfBlocks

block :: Block -> Info -> Info
block b i =
  i1 { iBlockInfo  = Map.insert (blockName b) newOwnership (iBlockInfo i1) }
  where
  owned = case Map.lookup (blockName b) (iBlockInfo i) of
            Nothing -> Set.empty
            Just ms -> Set.fromList
                     $ catMaybes
                     $ zipWith pick ms (blockArgs b)

  pick m a = case m of
               Owned -> Just a
               Borrowed -> Nothing

  i0 = i { iBlockOwned = owned }

  i1 = foldr ($) (cinstr (blockTerm b) i0)
     $ map instr (blockInstrs b)

  newOwnershipOf a = if a `Set.member` iBlockOwned i1 then Owned else Borrowed
  newOwnership     = map newOwnershipOf (blockArgs b)


instr :: Instr -> Info -> Info
instr i = foldr (.) id (zipWith expr (iArgs i) (modeI i))


cinstr :: CInstr -> Info -> Info
cinstr ci =
  case ci of
    Jump l -> jumpPoint l
    JumpIf _ ls  -> jumpChoice ls
    Yield        -> id
    ReturnNo     -> id
    ReturnYes e  -> expr e Owned
    ReturnPure e -> expr e Owned

    CallPure f l es ->
      \i -> jumpPoint l
          $ foldr ($) i
          $ zipWith expr es
          $ getFunOwnership f i

    Call f _ no yes es ->
      \i -> jumpPoint no
          $ jumpPoint yes
          $ foldr ($) i
          $ zipWith expr es
          $ getFunOwnership f i

    TailCall f _ es ->
      \i -> foldr ($) i
          $ zipWith expr es
          $ getFunOwnership f i

jumpChoice :: JumpChoice -> Info -> Info
jumpChoice (JumpCase opts) = \i -> foldr jumpWithFree i opts

jumpWithFree :: JumpWithFree -> Info -> Info
jumpWithFree = jumpPoint . jumpTarget

jumpPoint :: JumpPoint -> Info -> Info
jumpPoint (JumpPoint l es) i =
    foldr ($) i
  $ zipWith expr es
  $ getBlockOwnership l i

expr :: E -> Ownership -> Info -> Info
expr ex mo =
  case ex of
    EBlockArg x   -> addBlockArg x mo
    EUnit         -> id
    ENum {}       -> id
    EBool {}      -> id
    EMapEmpty {}  -> id
    ENothing {}   -> id
    EVar {}       -> id



modeI :: Instr -> [Ownership]
modeI i =
  case i of
    SetInput {}              -> [Owned]
    Say {}                   -> []
    Output _                 -> [Owned]
    Notify _                 -> [Owned] -- not ref
    CallPrim _ pn _          -> modePrimName pn
    GetInput _               -> []
    Spawn _ (JumpPoint _ es) -> zipWith const (repeat Owned) es
    NoteFail                 -> []
    Free {}                  -> []  -- XXX: `Free` owns its asrguments
    Let _ _                  -> [Borrowed] -- borrow to make a copy


modePrimName :: PrimName -> [Ownership]
modePrimName prim =
  case prim of
    StructCon {}  -> repeat Owned
    NewBuilder {} -> []
    Integer {}    -> []
    ByteArray {}  -> []
    Op1 op        -> modeOp1 op
    Op2 op        -> modeOp2 op
    Op3 op        -> modeOp3 op
    OpN op        -> modeOpN op

-- The mode only makes sense if the type is a reference.
modeOp1 :: Op1 -> [Ownership]
modeOp1 op =
  case op of
    CoerceTo {}           -> [Borrowed]
    CoerceMaybeTo {}      -> [Borrowed]
    IsEmptyStream         -> [Borrowed]
    Head                  -> [Borrowed]
    StreamOffset          -> [Borrowed]
    StreamLen             -> [Borrowed]
    OneOf {}              -> [Borrowed]
    Neg                   -> [Owned]
    BitNot                -> [Owned]
    Not                   -> [Owned]
    ArrayLen              -> [Borrowed]
    Concat                -> [Borrowed]
    FinishBuilder         -> [Owned]
    NewIterator           -> [Owned]
    IteratorDone          -> [Borrowed]
    IteratorKey           -> [Borrowed]
    IteratorVal           -> [Borrowed]
    IteratorNext          -> [Owned]
    EJust                 -> [Owned]
    FromJust              -> [Borrowed]
    SelStruct {}          -> [Borrowed]
    InUnion {}            -> [Owned]
    FromUnion {}          -> [Borrowed]

modeOp2 :: Op2 -> [Ownership]
modeOp2 op =
  case op of
    IsPrefix             -> [Borrowed,Borrowed]
    Drop                 -> [Borrowed,Owned]
    Take                 -> [Borrowed,Owned]

    Eq                   -> [Borrowed,Borrowed]
    NotEq                -> [Borrowed,Borrowed]
    Leq                  -> [Borrowed,Borrowed]
    Lt                   -> [Borrowed,Borrowed]

    Add                  -> [Owned,Owned]
    Sub                  -> [Owned,Owned]
    Mul                  -> [Owned,Owned]
    Div                  -> [Owned,Owned]
    Mod                  -> [Owned,Owned]

    BitAnd               -> [Owned,Owned]
    BitOr                -> [Owned,Owned]
    BitXor               -> [Owned,Owned]
    Cat                  -> [Owned,Owned]
    LCat                 -> [Owned,Borrowed]
    LShift               -> [Owned,Borrowed]
    RShift               -> [Owned,Borrowed]

    Or                   -> panic "modeOp2" ["Or"]
    And                  -> panic "modeOp2" ["And"]
    ArrayIndex           -> [Borrowed,Borrowed]
    ConsBuilder          -> [Owned,Owned]
    MapLookup            -> [Borrowed,Borrowed]
    MapMember            -> [Borrowed,Borrowed]

    ArrayStream          -> [Owned,Owned]

modeOp3 :: Op3 -> [Ownership]
modeOp3 op =
  case op of
    PureIf              -> panic "modeOp3" ["PureIf"]
    RangeUp             -> [Borrowed,Borrowed,Borrowed]
    RangeDown           -> [Borrowed,Borrowed,Borrowed]
    MapInsert           -> [Owned,Owned,Owned]

modeOpN :: OpN -> [Ownership]
modeOpN op =
  case op of
    ArrayL {} -> repeat Owned
    CallF {}  -> panic "modeOpN" [ "CallF" ]

