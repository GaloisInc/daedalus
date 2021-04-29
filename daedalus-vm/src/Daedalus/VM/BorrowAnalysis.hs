{-# Language OverloadedStrings, BlockArguments #-}
module Daedalus.VM.BorrowAnalysis(doBorrowAnalysis,modeI,modePrimName) where

import           Data.Maybe(catMaybes)
import           Data.Map(Map)
import qualified Data.Map as Map
import           Data.Set(Set)
import qualified Data.Set as Set

import Daedalus.Panic(panic)
import Daedalus.PP hiding (block)
import Daedalus.Core(Op1(..),Op2(..),Op3(..),OpN(..))
import Daedalus.VM
import Daedalus.VM.TypeRep

-- import Debug.Trace

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
     we plan to come back from the call (i.e., NOT in a tail-call or case),
     so that we can deallocate the variable.  If that variable is not used
     in the continuation, the continuation needs to be modified to
     free the variable upone return. See comments in `InsertCopy` for details.

  8. We can pass a variable we "own" as an "owned" argument to another function
     but then we give up ownership and should not use this variable anymore.

  9. Variables that are alive after a non-tail call (i.e., they are owned 
     by the stack allocated closure) do no impose a constraint on the
     called function

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
doBorrowAnalysis prog = prog { pEntries = annEntry  <$> pEntries prog
                             , pModules = annModule <$> pModules prog
                             }
  where
  info        = borrowAnalysis prog

  annEntry  e = e { entryBoot = annBlock <$> entryBoot e }
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
      Say {}          -> i
      Output e        -> Output (annE mp e)
      Notify e        -> Notify (annE mp e)
      CallPrim x p es -> CallPrim x p (map (annE mp) es)
      Spawn x l       -> Spawn x (annClo mp l)
      NoteFail e      -> NoteFail (annE mp e)
      Let x e         -> Let x (annE mp e)
      Free xs         -> Free (Set.map (annV mp) xs)

  annTerm mp t =
    case t of
      Jump l             -> Jump (annJ mp l)
      JumpIf e ls        -> JumpIf (annE mp e) (annJ2 mp ls)
      Yield              -> Yield
      ReturnNo           -> ReturnNo
      ReturnYes e i      -> ReturnYes (annE mp e) (annE mp i)
      ReturnPure e       -> ReturnPure (annE mp e)
      CallPure f l es    -> CallPure f (annClo mp l) (annE mp <$> es)
      Call f c no yes es -> Call f c (annClo mp no)
                                     (annClo mp yes) (annE mp <$> es)
      TailCall f c es    -> TailCall f c (annE mp <$> es)

  annVA mp v@(BA x _ _) = Map.findWithDefault v x mp

  annV mp v =
    case v of
      ArgVar a -> ArgVar (annVA mp a)
      LocalVar {} -> v

  annClo mp  c = annJ mp c --- XXX
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
    -- ^ Only meaningful while analyzing a block.  Contains block
    -- arguments that need to be owned for the current block.

  , iBlockInfo  :: Map Label [Ownership]
    -- ^ Ownership information for the arguments of existing blocks.

  , iFunEntry   :: Map FName Label
    -- ^ Entry block for a function.

  , iBlockType  :: Map Label BlockType
    -- ^ Information about what kind of block is this (return,thread,regular)
    -- as those use different calling conventions.

  , iChanges    :: Bool
    -- ^ Did we change anything?  Used to see if we reached a fixed point.
  }

-- | Add a constraint on a block argument
addBlockArg :: VMVar -> OwnInfo -> Info -> Info
addBlockArg y (prov,m) i =
  case m of
    Owned | ArgVar x <- y
          , not (x `Set.member` iBlockOwned i) ->
             -- trace ("Forcing " ++ showPP x ++ " to be owned")
             i { iChanges = True, iBlockOwned = Set.insert x (iBlockOwned i) }
    Borrowed | alreadyOwned
             , Just (l,n) <- prov ->
              -- trace ("Forcing argument " ++ show n ++ " of "
              -- ++ show (pp l) ++ " to be owned.") $
              i { iChanges = True
                , iBlockInfo =
                  Map.insert l
                    case splitAt n (map snd (getBlockOwnership l i)) of
                      (as,_:bs) -> as ++ Owned:bs
                      _ -> panic "addBlockArg" [ "bad arguments" ]
                    (iBlockInfo i)
                }
    _ -> i

  where
  alreadyOwned =
    case y of
      LocalVar {} -> True
      ArgVar x  -> x `Set.member` iBlockOwned i


setOwnership :: OwnInfo -> Info -> Info
setOwnership (mb,own) i =
  case mb of
    Nothing -> i
    Just (l,n) ->
      case splitAt (implicitArgs l i + n) (map snd (getBlockOwnership l i)) of
        (as,b:bs) | b /= own ->
          -- trace ("Changing " ++ show (pp l) ++ ":" ++ show n ++ " from " ++
          --           show b ++ " to " ++ show own)
          i { iChanges = True, iBlockInfo = Map.insert l (as ++ own : bs)
                                                         (iBlockInfo i) }
        _ -> i

-- Is the constraint from an argument of a block
type OwnInfo = (Maybe (Label,Int),Ownership)

getBlockOwnership :: Label -> Info -> [OwnInfo]
getBlockOwnership l i =
  case Map.lookup l (iBlockInfo i) of
    Just sig -> [ (Just (l,a),o) | (a,o) <- [ 0 .. ] `zip` sig ]
    Nothing  -> [ (Just (l,a),Borrowed) | a <- [ 0 .. ] ]

getFunOwnership :: FName -> Info -> [OwnInfo]
getFunOwnership f i =
  case Map.lookup f (iFunEntry i) of
    Just l  -> getBlockOwnership l i
    Nothing -> panic "getFunOwnership" [ "Missing entry point for " ++ show (pp f) ]

implicitArgs :: Label -> Info -> Int
implicitArgs l i =
  case Map.lookup l (iBlockType i) of
    Just ty -> extraArgs ty
    Nothing -> panic "implicitArgs" ["Missing block: " ++ show (pp l) ]

--------------------------------------------------------------------------------


borrowAnalysis :: Program -> Map Label [Ownership]
borrowAnalysis p = loop i0
  where
  i0 = Info { iBlockOwned = Set.empty
            , iBlockInfo  = Map.fromList $ (entryOwns <$> pEntries p)
                                        ++ (concatMap nonNormal (pAllBlocks p))
            , iChanges    = False
            , iFunEntry   = Map.fromList [ (vmfName f, vmfEntry f)
                                         | m <- pModules p, f <- mFuns m
                                         ]
            , iBlockType  = Map.fromList
                              [ (blockName b, blockType b) | b <- pAllBlocks p ]
            }

  loop i = let i1 = vmProgram p i
           in if iChanges i1
                then loop i1 { iChanges = False }
                else iBlockInfo i

  -- entries own their arguments
  entryOwns ent =
      let l = entryLabel ent
          args = blockArgs (entryBoot ent Map.! l)
      in (l, [ Owned `ifRefs` a | a <- args ])

  -- return and thread blocks own thier arguments
  nonNormal b =
    case blockType b of
      NormalBlock {} -> []
      _ -> [(blockName b, [ Owned `ifRefs` a | a <- blockArgs b ])]

vmProgram :: Program -> Info -> Info
vmProgram p i = foldr vmModule bs (pModules p)
  where
  bs = foldr vmEntry i (pEntries p)

vmEntry :: Entry -> Info -> Info
vmEntry e i = foldr block i (Map.elems (entryBoot e))

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
               Owned     -> Just a
               Borrowed  -> Nothing
               Unmanaged -> Nothing

  i0 = i { iBlockOwned = owned }

  i1 = foldr ($) (cinstr loc (blockTerm b) i0)
     $ map (instr loc) (blockInstrs b)

  loc = showPP (blockName b)

  newOwnershipOf a =
    (if a `Set.member` iBlockOwned i1 then Owned else Borrowed) `ifRefs` a

  -- return and thread blocks own their arguments
  newOwnership = map newOwnershipOf (blockArgs b)


instr :: String -> Instr -> Info -> Info
instr _b i =
  case i of
    Spawn _ l -> closure l
    _ -> foldr (.) id
                   $ zipWith expr (iArgs i) (zip (repeat Nothing) (modeI i))


cinstr :: String -> CInstr -> Info -> Info
cinstr _b ci =
  case ci of
    Jump l       -> jumpPoint l
    JumpIf _ ls  -> jumpChoice ls   -- the scrutinized expression is "borrowed"
    Yield        -> id
    ReturnNo     -> id
    ReturnYes e i -> expr e (Nothing,Owned `ifRefs` e)
                   . expr i (Nothing,Owned `ifRefs` i)
    ReturnPure e -> expr e (Nothing,Owned `ifRefs` e)

    CallPure f l es ->
      \i -> closure l
          $ foldr ($) i
          $ zipWith expr es
          $ getFunOwnership f i

    Call f _ no yes es ->
      \i -> closure no
          $ closure yes
          $ foldr ($) i
          $ zipWith expr es
          $ getFunOwnership f i

    TailCall f _ es ->
      \i -> foldr ($) i
          $ zipWith expr es
          $ getFunOwnership f i

closure :: Closure -> Info -> Info
closure clo i = foldr ($) upd
              $ zipWith expr (jArgs clo) args
  where
  args = [ (Just (jLabel clo,n), Owned `ifRefs` e)
                                          | (n,e) <- [0..] `zip` jArgs clo ]
  upd  = foldr ($) i (map setOwnership args)

jumpChoice :: JumpChoice -> Info -> Info
jumpChoice (JumpCase opts) = \i -> foldr jumpWithFree i opts

jumpWithFree :: JumpWithFree -> Info -> Info
jumpWithFree = jumpPoint . jumpTarget

jumpPoint :: JumpPoint -> Info -> Info
jumpPoint (JumpPoint l es) i =
    foldr ($) i
  $ zipWith expr es
  $ getBlockOwnership l i

expr :: E -> OwnInfo -> Info -> Info
expr ex mo =
  case ex of
    EBlockArg x   -> addBlockArg (ArgVar x) mo
    EVar x        -> addBlockArg (LocalVar x) mo
    EUnit         -> id
    ENum {}       -> id
    EBool {}      -> id
    EMapEmpty {}  -> id
    ENothing {}   -> id



modeI :: Instr -> [Ownership]
modeI i =
  case i of
    Say {}                   -> []
    Output e                 -> [ Owned `ifRefs` e ]
    Notify _                 -> [ Unmanaged ]
    CallPrim _ pn es         -> zipWith ifRefs (modePrimName pn) es
    Spawn _ clo              -> map (ifRefs Owned) (jArgs clo)
    NoteFail e               -> [ Borrowed `ifRefs` e ]
    Free {}                  -> []  -- XXX: `Free` owns its asrguments
    Let _ e                  -> [ Borrowed `ifRefs` e] -- borrow to make a copy


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

    ArrayIndex           -> [Borrowed,Borrowed]
    ConsBuilder          -> [Owned,Owned]
    MapLookup            -> [Borrowed,Borrowed]
    MapMember            -> [Borrowed,Borrowed]

    ArrayStream          -> [Owned,Owned]

modeOp3 :: Op3 -> [Ownership]
modeOp3 op =
  case op of
    RangeUp             -> [Borrowed,Borrowed,Borrowed]
    RangeDown           -> [Borrowed,Borrowed,Borrowed]
    MapInsert           -> [Owned,Owned,Owned]

modeOpN :: OpN -> [Ownership]
modeOpN op =
  case op of
    ArrayL {} -> repeat Owned
    CallF {}  -> panic "modeOpN" [ "CallF" ]

