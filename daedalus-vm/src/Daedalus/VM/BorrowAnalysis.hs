{-# Language OverloadedStrings #-}
module Daedalus.VM.BorrowAnalysis(doBorrowAnalysis) where

import           Data.Maybe(catMaybes)
import           Data.Map(Map)
import qualified Data.Map as Map
import           Data.Set(Set)
import qualified Data.Set as Set

import Daedalus.Panic(panic)
import Daedalus.PP hiding (block)
import Daedalus.Core(FName,Op1(..),Op2(..),Op3(..),OpN(..))
import Daedalus.VM



doBorrowAnalysis :: Program -> Program
doBorrowAnalysis prog = prog { pModules = annModule <$> pModules prog }
  where
  info        = borrowAnalysis prog
  annModule m = m { mFuns     = annFun   <$> mFuns m }
  annFun f    = f { vmfBlocks = annBlock <$> vmfBlocks f }
  annBlock b =
    case Map.lookup (blockName b) info of
      Just sig -> b { blockArgs = zipWith annArg (blockArgs b) sig }
      Nothing  -> panic "doBorrowAnalysis"
                    [ "Missing ownership information for"
                    , show (pp (blockName b))
                    ]
  annArg (BA x t _) own = BA x t own






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
           in if iChanges i1 then loop i1 { iChanges = False } else iBlockInfo i


vmProgram :: Program -> Info -> Info
vmProgram = foldr (.) id . map vmModule . pModules

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
instr i =
  case i of
    SetInput e               -> expr e Owned
    Say {}                   -> id
    Output e                 -> expr e Owned
    Notify e                 -> id -- thread id is not a reference
    CallPrim pn es x         -> foldr (.) id (zipWith expr es (modePrimName pn))
    GetInput x               -> id
    Spawn (JumpPoint _ es) x -> foldr (.) id (zipWith expr es (repeat Owned))
    NoteFail                 -> id
    Free {}                  -> id    -- do not consider?

cinstr :: CInstr -> Info -> Info
cinstr ci =
  case ci of
    Jump l -> jumpPoint l
    JumpIf _ l1 l2 -> jumpPoint l1 . jumpPoint l2
    Yield -> id
    ReturnNo -> id
    ReturnYes e -> expr e Owned
    Call f _ l1 l2 es ->
      \i -> jumpPoint l1
          $ jumpPoint l2
          $ foldr ($) i
          $ zipWith expr es
          $ getFunOwnership f i

    TailCall f _ es ->
      \i -> foldr ($) i
          $ zipWith expr es
          $ getFunOwnership f i
    ReturnPure e -> expr e Owned



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
    EByteArray {} -> id
    EMapEmpty {}  -> id
    ENothing {}   -> id
    EVar {}       -> id





modePrimName :: PrimName -> [Ownership]
modePrimName prim =
  case prim of
    StructCon {}  -> repeat Owned
    NewBuilder {} -> []
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
    Concat                -> [Owned]
    FinishBuilder         -> [Borrowed]
    NewIterator           -> [Owned]
    IteratorDone          -> [Borrowed]
    IteratorKey           -> [Borrowed]
    IteratorVal           -> [Borrowed]
    IteratorNext          -> [Owned]
    EJust                 -> [Owned]
    IsJust                -> [Borrowed]
    FromJust              -> [Borrowed]
    SelStruct {}          -> [Borrowed]
    InUnion {}            -> [Owned]
    HasTag {}             -> [Borrowed]
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

    ArrayStream          -> [Borrowed,Borrowed]

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
    CallF {}  -> repeat Borrowed  -- character classes have no refs?


