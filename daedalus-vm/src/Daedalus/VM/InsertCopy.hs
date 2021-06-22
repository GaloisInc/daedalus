-- | Assumes borrow analysis has been done
{-# Language BlockArguments, OverloadedStrings, RecordWildCards #-}
module Daedalus.VM.InsertCopy (addCopyIs) where

import Control.Monad(zipWithM,ap,liftM)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.List(mapAccumL)

import Daedalus.PP(pp)
import Daedalus.Panic(panic)

import Daedalus.VM
import Daedalus.VM.BorrowAnalysis
import Daedalus.VM.TypeRep
import Daedalus.VM.FreeVars


addCopyIs :: Program -> Program
addCopyIs p = p { pEntries = annEntry  <$> pEntries p
                , pModules = annModule <$> pModules p
                }
  where
  annEntry e  = e { entryBoot = blockMap (entryBoot e) }
  annModule m = m { mFuns = map annFun (mFuns m) }
  annFun f    = f { vmfDef = annDef (vmfDef f) }
  annDef d    = case d of
                  VMExtern {} -> d
                  VMDef b -> VMDef b { vmfBlocks = blockMap (vmfBlocks b) }
  ro          = buildRO p
  blockMap    = Map.fromList
              . map (\b -> (blockName b, b))
              . concatMap (annotateBlock ro)
              . Map.elems

buildRO :: Program -> RO
buildRO p = foldr addFun initRO [ f | m <- pModules p, f <- mFuns m ]
  where
  initRO = RO { funMap = Map.empty
              , labOwn = Map.unions [ blockSig <$> entryBoot entry
                                                        | entry <- pEntries p ]
              }

  addFun f i =
    case vmfDef f of
      VMDef d -> RO { funMap = Map.insert (vmfName f) fs (funMap i)
                    , labOwn = Map.union ls (labOwn i)
                    }
        where
        ls = blockSig <$> vmfBlocks d
        fs = fst (ls Map.! vmfEntry d)
      VMExtern as -> i { funMap = Map.insert (vmfName f) fs (funMap i) }
        where fs = map getOwnership as

  blockSig b = (map getOwnership (blockArgs b), blockType b)


annotateBlock :: RO -> Block -> [Block]
annotateBlock ro = map rmRedundantCopy . insertFree ro . insertCopy ro


--------------------------------------------------------------------------------

rmRedundantCopy :: Block -> Block
rmRedundantCopy b = b { blockInstrs = is, blockTerm = c }
  where
  (is,c) = doRmRedundat [] (blockInstrs b) (blockTerm b)



{-
Optimizaiton 1:
  x = copy y
  free y
  k
~>
  k [y/x]

Optimizaiton 2:
  x = copy y
  jump choice L R
     * x and y are not both used in L
     * x and y are not both used in R
  ~>
  jump choice L[y/x] R[y/x] and remove "y" from free, unless "x" was
  in the free.

  NOTE:  the order of the copies is arbitrary so the optimization should
  work even there are other copies between it and the jump

Example 1:

jump choice L(x) R(x)
~>
x1 = copy x
x2 = copy x
free x
jump choice L(x1,free(x2)) R(x2, free(x1))
~> [1st optimization]
x1 = copy x
jump choice L(x1,free(x)) R(x, free(x1))
~> [2nd optimization]
jump choice L(x) R(x)

Example 2:

jump choice L(x) R(x,x)
~>
x1 = copy x
x2 = copy x
x3 = copy x
free x
jump choice L(x1,free(x2,x3)) R(x2,x3, free(x1))
~> [1st optimization]
x1 = copy x
x2 = copy x
jump choice L(x1,free(x2,x)) R(x2,x, free(x1))
  * x2 and x are both in RHS so can't remove `x2 = copy x`
  * x1 and x are not in eithrt side so we can remove copy:
~>
x2 = copy
jump choice L(x,free(x2)) R(x2,x)


XXX: another case:
  x = copy y
  f(x)      --- x is borrowed
    L_free(y,x)
-->
  f(y)
    L(y)
-}


doRmRedundat :: [Instr] -> [Instr] -> CInstr -> ([Instr],CInstr)
doRmRedundat doneIs is term =
  case is of
    -- Optimization 1: Make a copy, and immediately dallocate source
    Let x e : Free xs : more
      | Just y <- eIsVar e, y `Set.member` xs
      , let xs' = Set.delete y xs ->
        doRmRedundat doneIs (doSubst x y (Free xs' : more)) (doSubst x y term)

    Free xs : more
      | Set.null xs -> doRmRedundat doneIs more term
      | Free ys : ps <- doneIs ->
        doRmRedundat (Free (Set.union xs ys) : ps) more term

    i : more -> doRmRedundat (i : doneIs) more term

    [] -> checkTermCopies doneIs term


{- Check for Optimization 2 `x = copy y` can be eliminated if `x` and `y`
will never exist at the same time.

Assumes all "copy" was inserted by the previous pass.
In particular, we should never see:
let x = y
let z = x

Note, however, that "free copies" may occur in the branches of a case,
so we have to be careful there.
-}
checkTermCopies :: [Instr] -> CInstr -> ([Instr], CInstr)
checkTermCopies is0 term0 = (reverse is1, term1)
  where
  (is1,term1) =
    case term0 of
      JumpIf e ls0 ->
        case checkIs okChoice is0 ls0 of
          (is,ls) -> (is, JumpIf e ls)

      _ -> (is0,term0)

  checkIs upd is jc =
    case is of
      i@(Let x e) : more ->
        case eIsVar e of
          Just v
            | Just jc1 <- upd x v jc -> checkIs upd more jc1
          _ -> let (is',jc1) = checkIs upd more jc
               in (i : is',jc1)

      _ -> (is, jc)

  okChoice :: BV -> VMVar -> JumpChoice -> Maybe JumpChoice
  okChoice x' v (JumpCase opts)
    | all ok optsVs = Just (JumpCase (elimJF <$> opts))
    | otherwise     = Nothing

    where
    x         = LocalVar x'

    ok vs     = not (x `Set.member` vs && v `Set.member` vs)

    optsVs = (freeVarSet . jumpTarget) <$> opts

    elimJF jf = JumpWithFree
                  { jumpTarget = doSubst x' v (jumpTarget jf)
                  , freeFirst =
                     let toFree = freeFirst jf
                     in Set.delete x
                      $ if x `Set.member` toFree
                          then toFree
                          else Set.delete v toFree
                  }




-- | Replace a local variable with another variable.
-- The new variable is one that we know should not be freed explicitly,
-- because it's being transfarred to a new owner.
class DoSubst t where
  doSubst :: BV -> VMVar -> t -> t

instance DoSubst t => DoSubst [t] where
  doSubst x e = fmap (doSubst x e)

instance DoSubst t => DoSubst (Maybe t) where
  doSubst x e = fmap (doSubst x e)

instance DoSubst Instr where
  doSubst x v i =
    case i of
      Say {}          -> i
      Output e        -> Output (doSubst  x v e)
      Notify e        -> Notify (doSubst  x v e)
      CallPrim y p es -> CallPrim y p (doSubst x v es)
      Spawn y l       -> Spawn y (doSubst x v l)
      NoteFail e      -> NoteFail (doSubst x v e)
      Let y e         -> Let y (doSubst x v e)
      Free xs         -> Free (Set.delete (LocalVar x) (Set.delete v xs))

instance DoSubst CInstr where
  doSubst x v ci =
    case ci of
      Jump l          -> Jump (doSubst x v l)
      JumpIf e ls     -> JumpIf (doSubst x v e) (doSubst x v ls)
      Yield           -> Yield
      ReturnNo        -> ReturnNo
      ReturnYes e i   -> ReturnYes  (doSubst x v e) (doSubst x v i)
      ReturnPure e    -> ReturnPure (doSubst x v e)
      CallPure f l es -> CallPure f (doSubst x v l) (doSubst x v es)
      Call f c no yes es -> Call f c (doSubst x v no) (doSubst x v yes)
                                                      (doSubst x v es)
      TailCall f c es -> TailCall f c (doSubst x v es)

instance DoSubst JumpPoint where
  doSubst x v (JumpPoint l es) = JumpPoint l (doSubst x v es)

instance DoSubst JumpChoice where
  doSubst x v (JumpCase opts) = JumpCase (doSubst x v <$> opts)

instance DoSubst JumpWithFree where
  doSubst x v jf =
    JumpWithFree
      { freeFirst = let fs = freeFirst jf
                        x' = LocalVar x
                    in if x' `Set.member` fs
                          then Set.insert v (Set.delete x' fs)
                          else fs
      , jumpTarget = doSubst x v (jumpTarget jf)
      }


instance DoSubst E where
  doSubst x v e =
    case e of
      EVar y | x == y -> case v of
                           LocalVar z -> EVar z
                           ArgVar z   -> EBlockArg z
      _               -> e



--------------------------------------------------------------------------------

{- | Insert @free@ after the last use of owned variables that have references.
  * We do not insert @free@ instructions for copies inserted in the previous
    pass as those are going to be freed by their new owners.
  * BUT: we do insert them in the branches of the case as only one
         of them will be executed, so we need to clean-up before proceeding.
 -}
insertFree :: RO -> (Set BV, Block) -> [Block]
insertFree ro (copies,b) = b { blockInstrs = newIs, blockTerm = newTerm }
                         : newBlocks
  where
  (newTerm,newBlocks) = inTerm

  (finLive,iss) = mapAccumL updI (freeVarSet newTerm)
                                 (reverse (blockInstrs b))
  baSet      = Set.fromList [ ArgVar v | v <- blockArgs b ]
  topFreeIs  = mkFree (Set.difference baSet finLive)
  newIs      = topFreeIs ++ concat (reverse iss)

  updI :: Set VMVar -> Instr -> (Set VMVar, [Instr])
  updI live i = (newLive, i : freeIs)
    where
    used     = freeVarSet i
    newLive  = Set.union used live `Set.difference` defs
    defs     = LocalVar `Set.map` defineSet i
    freeIs   = mkFree (Set.difference (Set.union defs used) live)


  mkFree vs = [ Free vs' | let vs'= filterFree True vs, not (Set.null vs') ]

  filterFree checkCopy = Set.filter \v -> getOwnership v == Owned &&
                                typeRep (getType v) == HasRefs &&
                                (not checkCopy ||
                                 case v of
                                   LocalVar y -> not (y `Set.member` copies)
                                   ArgVar {}  -> True)


  inTerm = case blockTerm b of
             JumpIf e ls -> (JumpIf e (freeChoice e ls), [])

             CallPure f l es
                | not (null cs) -> (CallPure f l' es, [newB])
                where cs = checkFun f es [l]
                      (l',newB) = makeOwner l cs

             Call f c l1 l2 es
                | not (null cs) -> (Call f c l' l2 es,[newB])
                where cs = checkFun f es [l1,l2]
                      (l',newB) = makeOwner l1 cs  -- arbitrary choice of l1

             t -> (t,[])

  freeChoice e (JumpCase opts) = JumpCase (Map.mapWithKey doCase opts)
    where
    allFree = getFree <$> opts

    doCase l it =
      let (this, rest) = doLookupRm l allFree
          others'      = Set.unions (Map.elems rest)
          others       = case eIsVar e of
                           Just x | getOwnership x == Owned ->
                                                      Set.insert x others'
                           _ -> others'
      in changeFree (others `Set.difference` this) it

    changeFree vs x = x { freeFirst = filterFree False vs }
    getFree         = freeVarSet . jumpTarget

  -- if we call f(x) and `x` expects borrowed, but we are passing it owned,
  -- upon returning we need to free `x`
  --  * if `x` is in at least one of the continuation's closures we are done
  --    (the continuation is the owner that will take care of it).
  --  * if not, then we need to add it by making an extra block:
  --      B: (thingsToFree,otherArgs)
  --          free(thingsToFree)
  --          goto L(otherArgs)   -- where L was the original return address
  --  * For parsers, where we have 2 return continuations, we only need to
  --    modify one of the blocks as above, and this block becomes the new
  --    owner.
  checkFun f es ls =
    case Map.lookup f (funMap ro) of
      Just sig -> filter (needsOwner ls) (concat (zipWith checkArg sig es))
      Nothing  -> panic "funOwn" [ "Missing function " ++ show (pp f) ]

  -- an owned argument that is borrowed by the call
  checkArg funParam e =
    case eIsVar e of
      Just v | funParam == Borrowed && getOwnership v == Owned -> [v]
      _ -> []

  needsOwner ls c = typeRep (getType c) == HasRefs && not (any ownedBy ls)
    where
    ownedBy l = any matches (jArgs l)
      where
      matches e = Just c == eIsVar e

  makeOwner l cs =
    let nm = case jLabel l of
               Label txt n -> Label ("_free_" <> txt) n
        toArg n e = BA n (getType e) Owned
        oldArgs = zipWith toArg [ 0 .. ] (jArgs l)
        newArgs = zipWith toArg [ length oldArgs .. ] cs
    in ( JumpPoint
            { jLabel = nm
            , jArgs  = jArgs l ++ map eVar cs
            }
       , Block
          { blockName     = nm
          , blockType     = case Map.lookup (jLabel l) (labOwn ro) of
                              Just (_,ty) -> ty
                              Nothing -> panic "makeOwner"
                                          [ "Missing block: " ++ show (pp l) ]
          , blockArgs     = oldArgs ++ newArgs
          , blockLocalNum = 0
          , blockInstrs   = [ Free (Set.fromList (map ArgVar newArgs)) ]
          , blockTerm     = Jump JumpPoint
                                   { jLabel = jLabel l
                                   , jArgs  = map EBlockArg oldArgs
                                   }
          }
       )




doLookupRm :: Ord k => k -> Map k v -> (v,Map k v)
doLookupRm k mp = (v,mp1)
  where
  (Just v,mp1)  = Map.updateLookupWithKey del k mp
  del _ _       = Nothing

--------------------------------------------------------------------------------

insertCopy :: RO -> Block -> (Set BV, Block)
insertCopy ro b = ( vs
                  , b { blockLocalNum = newLocalNum
                      , blockInstrs = is
                      , blockTerm = cinstr
                      }
                  )
  where
  (cinstr,newLocalNum,vs,is) =
     runM ro (blockLocalNum b)
     do mapM_ doInstr (blockInstrs b)
        doCInstr (blockTerm b)

-- Insert a copy instructions whenever something exepcts and owned argument,
-- and the type contains refs.
doInstr :: Instr -> M ()
doInstr instr =
  case instr of
    Say {}          -> emit instr
    Output e        -> owned Output e
    Notify {}       -> emit instr
    CallPrim x p es ->
      do es1 <- doArgs es (modePrimName p)
         emit (CallPrim x p es1)

    Spawn x (JumpPoint l es) ->   -- this is a bit different because we
                                  -- are not jumping now
      do es1 <- mapM copy es
         emit (Spawn x (JumpPoint l es1))
    NoteFail {}     -> emit instr -- borrows
    Let {}          -> emit instr
    Free {}         -> emit instr

  where
  owned f e =
    do e1 <- copy e
       emit (f e1)

doArgs :: [E] -> [Ownership] -> M [E]
doArgs es ms = zipWithM doArg es ms

doArg :: E -> Ownership -> M E
doArg e m =
  case m of
    Owned     -> copy e
    Borrowed  -> pure e
    Unmanaged -> pure e



doCInstr :: CInstr -> M CInstr
doCInstr cinstr =
  case cinstr of
    Jump l       -> Jump <$> doJump l
    JumpIf e ls  -> JumpIf e <$> doJumpChoice ls

    Yield        -> pure cinstr
    ReturnNo     -> pure cinstr
    ReturnYes e i-> ReturnYes  <$> copy e <*> copy i
    ReturnPure e -> ReturnPure <$> copy e

    CallPure f l es ->
      do sig <- lookupFunMode f
         es1 <- doArgs es sig
         l1  <- doJump l
         pure (CallPure f l1 es1)

    Call f c no yes es  ->
      do sig <- lookupFunMode f
         es1 <- doArgs es sig
         no1 <- doJump no
         yes1 <- doJump yes
         pure (Call f c no1 yes1 es1)

    TailCall f c es ->
      do sig <- lookupFunMode f
         es1 <- doArgs es sig
         pure (TailCall f c es1)


doJump :: JumpPoint -> M JumpPoint
doJump (JumpPoint l es) =
  do ms <- lookupLabelMode l
     JumpPoint l <$> doArgs es ms

-- | We will only take one of the two choices at run time.
-- So when we add a `copy` for the one side we add a `free` to the other.
-- A later pass removes redundant copies
doJumpChoice :: JumpChoice -> M JumpChoice
doJumpChoice (JumpCase opts) =
  do opts' <- mapM doSide opts
     let doOne l (it,_) =
          addVs (Set.unions $ map snd $ Map.elems $ Map.delete l opts') it
     pure (JumpCase (Map.mapWithKey doOne opts'))
  where
  addVs xs jf = jf { freeFirst = Set.union xs (freeFirst jf) }
  doSide jf =
    do (t,vs) <- observeCopies (doJump (jumpTarget jf))
       pure (jf { jumpTarget = t }, Set.fromList (map LocalVar vs))



copy :: E -> M E
copy e =
  case eIsVar e of
    Nothing -> pure e
    _ -> case typeRep ty of
           NoRefs  -> pure e
           HasRefs ->
             do x <- newBV ty
                emit (Let x e)
                pure (EVar x)
  where
  ty = getType e


--------------------------------------------------------------------------------
newtype M a = M (RO -> RW -> (a,RW))

runM :: RO -> Int -> M a -> (a,Int,Set BV,[Instr])
runM ro v (M m) = ( a
                  , nextName s1
                  , Set.fromList (concat (newVars s1))
                  , reverse (revInstr s1)
                  )
  where
  (a,s1) = m ro RW { nextName = v, newVars = [[]], revInstr = [] }

data RO = RO { funMap :: Map FName [Ownership]
             , labOwn :: Map Label ([Ownership], BlockType)
             }

data RW = RW { nextName :: Int
             , revInstr :: [Instr]
             , newVars  :: [[BV]]
             }

instance Functor M where
  fmap = liftM

instance Applicative M where
  pure a = M \_ s -> (a,s)
  (<*>)  = ap

instance Monad M where
  M m >>= f = M \r s -> case m r s of
                          (a,s1) ->
                            case f a of
                              M m1 -> m1 r s1

observeCopies :: M a -> M (a, [BV])
observeCopies (M m) = M \r s ->
  case m r s { newVars = [] : newVars s } of
    (a,s1) -> let vs : y : xs = newVars s1
              in ((a,vs), s1 { newVars = (vs ++ y) : xs })

emit :: Instr -> M ()
emit i = M \_ s -> ((), s { revInstr = i : revInstr s })

newBV :: VMT -> M BV
newBV t = M \_ s ->
  let x = nextName s
      v = BV x t
  in (v, s { nextName = x + 1
           , newVars  = let a : as = newVars s
                        in (v:a) : as
           })

lookupFunMode :: FName -> M [Ownership]
lookupFunMode f = M \r s ->
  case Map.lookup f (funMap r) of
    Just ms -> (ms,s)
    Nothing -> panic "lookupFunMode" ["Missing mode for " ++ show (pp f)]

lookupLabelMode :: Label -> M [Ownership]
lookupLabelMode l = M \r s ->
  case Map.lookup l (labOwn r) of
    Just ms -> (fst ms,s)
    Nothing -> panic "lookupLabelMode" ["Missing signature for " ++ show (pp l)]
