{-# Language BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# Language OverloadedStrings #-}
-- | This module desugars looping constructs into recursive functions.
module Daedalus.Core.NoLoop (noLoop) where


import           MonadLib.Monads

import Daedalus.GUID(HasGUID (guidState), GUID, mkGUIDState, invalidGUID)
import Daedalus.Core
import Daedalus.Core.Type(sizeType, typeOf, TypeOf)
import Daedalus.Panic (panic)
import Data.Text (Text)
import qualified Data.Map as Map
import Daedalus.Core.Rename (rename, renameIgnoring, Rename)
import qualified Data.ByteString.Char8 as BS8

-- -----------------------------------------------------------------------------
-- Monad

newtype M a = M (State S a)
  deriving (Functor,Applicative,Monad)

data S = S
  { newFFuns  :: [Fun Expr]       -- ^ Generated pure functions
  , newGFuns  :: [Fun Grammar]    -- ^ Generated parsing functions
  , curMod    :: MName            -- ^ Current module (doesn't change)
  , nextGUID  :: GUID
    -- ^ used for name generation
  }

instance HasGUID M where
  guidState f = M $ mkGUIDState nextGUID (\g' s -> s { nextGUID = g'}) f

-- | Run a translation
run :: HasGUID m => M a -> m (a, [Fun Grammar] , [Fun Expr])
run (M m) = guidState go
  where
    initS = S [] [] (error "No current module")
    go g =
      let (r, s) = runState (initS g) m
      in ((r, newGFuns s, newFFuns s), nextGUID s)

-- | Process the body of a function
inFun :: (a -> M a) -> Fun a -> M (Fun a)
inFun go f = case fDef f of
  Def b    -> do
    setMName
    r <- go b
    pure (f { fDef = Def r })
  External -> pure f
  where
    -- Should really be in Reader, but we don't nest so having stale
    -- mod names isn't an issue
    setMName = M $ sets_ (\s -> s { curMod = fnameMod (fName f) })

-- | Make a fresh name derived from the optionally given name,
-- and with the given type
newName :: Maybe Text -> Type -> M Name
newName mb t =
  freshName Name { nameId = invalidGUID, nameType = t, nameText = mb }

-- | Make up a new local name
newLocal :: Type -> M Name
newLocal = newName Nothing

-- | Ensure that an expression is in variable form.
-- If it is not already, then we name the expression.
withVar :: CoreSyn t => Expr -> (Name -> M t) -> M t
withVar e k =
  case e of
    Var x -> k x
    _     -> do x <- newLocal (typeOf e)
                coreLet x e <$> k x

-- | Used to name expressions when they may be duplicated.
withNamedExpr :: CoreSyn t => Expr -> (Expr -> M t) -> M t
withNamedExpr e k =
  case e of
    -- We don't name constants
    Ap0 {} -> k e
    -- ... otherwise we do (Vars are handled in withVar)
    _      -> withVar e (k . Var)

-- | Construct a choice expression for the given backtracking strategy.
orOp :: Backtrack -> Grammar -> Grammar -> Grammar
orOp cmt = case cmt of
             Eager -> OrBiased
             Lazy  -> OrUnbiased

-- | Report an error with the given message.  This is a "system" error
-- in the sense that we synthesiszed it (i.e., it is not a user specified
-- error message)
sysErr :: Type -> String -> Grammar
sysErr t msg = Fail ErrorFromSystem t (Just (byteArrayL (BS8.pack msg)))

doIf :: CoreSyn t => Expr -> t -> t -> M t
doIf e g1 g2 = withVar e \x -> pure (coreIf x g1 g2)


newFName :: Text -> Type -> M FName
newFName lab ty = do
  cmod <- M $ curMod <$> get
  freshFName
    FName { fnameId   = invalidGUID
          , fnameType = ty
          , fnameText = lab
          , fnamePublic = False
          , fnameMod  = cmod
          }

mkFun :: FName -> [Name] -> a -> Fun a
mkFun fname anames b = 
  Fun { fName = fname
      , fParams = anames
      , fDef = Def b
      , fIsEntry = False
      , fAnnot = [] -- FIXME
      }

defGFun :: FName -> [Name] -> Grammar -> M ()
defGFun fname anames g =
  M $ sets_ (\s -> s { newGFuns = mkFun fname anames g : newGFuns s })

defFFun :: FName -> [Name] -> Expr -> M ()
defFFun fname anames g =
  M $ sets_ (\s -> s { newFFuns = mkFun fname anames g : newFFuns s })

-- Names a grammar (in a function) so we don't duplicate terms.  In
-- theory this could not name for simple bodies, but for now we just
-- name everything.
nameGrammar :: Text -> Grammar -> M Grammar
nameGrammar name g =
  do (g', nameMap) <- rename g
     let ty = typeOf g
         (args, anames) = unzip (Map.toList nameMap)
     fname <- newFName name ty
     defGFun fname anames g'
     pure (Call fname (map Var args))

-- -----------------------------------------------------------------------------
-- Entry

noLoop :: HasGUID m => Module -> m Module
noLoop mo = do
  ((gs, es, bs), newgs, newes) <-
    run $ (,,) <$> mapM (inFun noLoopG) (mGFuns mo)
               <*> mapM (inFun noLoopE) (mFFuns mo)
               <*> mapM (inFun noLoopB) (mBFuns mo)
  pure mo { mGFuns = gs ++ newgs
          , mFFuns = es ++ newes
          , mBFuns = bs
          }

noLoopE :: Expr -> M Expr
noLoopE e =
  do e' <- childrenE noLoopE e
     case e' of
       ELoop lm -> noLoopMorphism lm defFFun id coreLet
       _ -> pure e'

noLoopB :: ByteSet -> M ByteSet
noLoopB = ebChildrenB noLoopE noLoopB

noLoopG :: Grammar -> M Grammar
noLoopG g =
  do g1 <- gebChildrenG noLoopG noLoopE noLoopB g
     case g1 of
       Loop (ManyLoop s c l m_u b) -> noManyLoop s c l m_u b
       Loop (RepeatLoop c x e b)   -> noRepeatLoop c x e b
       Loop (MorphismLoop lm)      -> noLoopMorphism lm defGFun Pure Do
       _       -> pure g1

-- -----------------------------------------------------------------------------
-- Many

-- FIXME: we could treat a lower bound of 0 specially here
noManyLoop :: Sem -> Backtrack -> Expr -> Maybe Expr ->
              Grammar -> M Grammar
noManyLoop s cmt l m_u p =
  case (m_u, s) of

    -- No upper bound
    (Nothing, _) ->
      do p' <- nameGrammar "ManyBody" p
         case s of
           SemNo -> Do_ <$> pSkipExactlyMany cmt l p'
                        <*> pSkipMany cmt p'
           SemYes -> do
             b <- newLocal (TBuilder ty)
             p1 <- pParseExactlyMany cmt ty l p'
             p2 <- pParseMany cmt ty (Var b) p'
             finishMany ty (Do b p1 p2)

    -- Exact
    (Just u, SemNo)  | l == u -> pSkipExactlyMany cmt l p
    (Just u, SemYes) | l == u -> pParseExactlyMany cmt ty l p
                                 >>= finishMany ty
    (Just u, _) ->
      withNamedExpr l \le ->
      withNamedExpr u \ue ->
        doIf (ue `lt` le)
             (sysErr (TArray ty) "Empty bounds")
             =<<
             do p' <- nameGrammar "ManyBody" p
                case s of
                  SemNo -> Do_ <$> pSkipExactlyMany cmt le p'
                               <*> pSkipAtMost cmt (ue `sub` le) p'
                  SemYes -> do
                    b <- newLocal (TBuilder ty)
                    p1 <- pParseExactlyMany cmt ty le p'
                    p2 <- pParseAtMost cmt ty (ue `sub` le) (Var b) p'
                    finishMany ty (Do b p1 p2)
  where
    ty = typeOf p

maybeSkip :: Backtrack -> Grammar -> Grammar -> Grammar -> M Grammar
maybeSkip cmt p yes no =
  do r <- newLocal TBool
     pure
       case cmt of
         Eager ->
            Do r (OrBiased (Do_ p (Pure (boolL True))) (Pure (boolL False)))
            $ coreIf r yes no
         Lazy -> orOp cmt (Do_ p yes) no


maybeParse ::
  Backtrack -> Type -> Grammar -> (Expr -> Grammar) -> Grammar -> M Grammar
maybeParse cmt ty p yes no =
  do r   <- newLocal ty
     rMb <- newLocal (TMaybe ty)
     pure
       case cmt of
         Eager ->
            Do rMb (OrBiased (Do r p (Pure (just (Var r))))
                                (Pure (nothing ty)))
            $ GCase
            $ Case rMb
                [ (PJust,    yes (eFromJust (Var rMb)))
                , (PNothing, no)
                ]
         Lazy -> orOp cmt (Do r p (yes (Var r))) no

pSkipMany :: Backtrack -> Grammar -> M Grammar
pSkipMany cmt p =
  do f <- newFName "Many" TUnit
     (p', nameMap) <- rename p
     let (anames, args) = unzip (Map.toList nameMap)
     skipBody <- maybeSkip cmt p' (Call f (map Var args)) (Pure unit)
     defGFun f args skipBody
     pure (Call f (map Var anames))

pParseMany :: Backtrack -> Type -> Expr -> Grammar -> M Grammar
pParseMany cmt ty be p =
  do f <- newFName "Many" (TBuilder ty)
     (p', nameMap) <- rename p
     let (anames, args) = unzip (Map.toList nameMap)
     x <- newLocal (TBuilder ty)
     let xe = Var x
     body <- maybeParse cmt ty p'
                              (\a -> Call f (emit xe a : map Var args)) (Pure xe)
     defGFun f (x:args) body
     pure $ Call f (be : map Var anames)

pSkipExactlyMany :: Backtrack -> Expr -> Grammar -> M Grammar
pSkipExactlyMany _cmt (Ap0 (IntL 0 _)) _ = pure (Pure unit)
pSkipExactlyMany _cmt tgt p =
  do f <- newFName "Many" TUnit
     ((tgt', p'), nameMap) <- rename (tgt, p)
     let (anames, args) = unzip (Map.toList nameMap)
     x <- newLocal sizeType
     let xe = Var x
     let body = Do_ (OrBiased p' (sysErr TUnit "insufficient element occurances"))
                    (Call f (add xe (intL 1 sizeType) : map Var args))

     defGFun f (x : args) =<< doIf (xe `lt` tgt') body (Pure unit)
     pure $ Call f (intL 0 sizeType : map Var anames)

-- | Produces a function which returns a builder

-- FIXME: we should evaluate tgt outside of the function if it is
-- complex to avoid recomputing it.
pParseExactlyMany :: Backtrack -> Type -> Expr -> Grammar -> M Grammar
pParseExactlyMany _cmt ty (Ap0 (IntL 0 _)) _ = pure (Pure (newBuilder ty))
pParseExactlyMany _cmt ty tgt p =
  do f <- newFName "Many" (TBuilder ty)
     ((tgt', p'), nameMap) <- rename (tgt, p)
     let (anames, args) = unzip (Map.toList nameMap)
     x <- newLocal sizeType
     b <- newLocal (TBuilder ty)
     r <- newLocal ty

     let xe = Var x
         be = Var b
         re = Var r

     -- FIXME: We don't need to worry about commit here(?) as we 
     -- always take exactly tgt many iterations
     let body = Do r (OrBiased p' (sysErr ty "insufficient element occurances"))
                     (Call f (add xe (intL 1 sizeType)
                              : emit be re
                              : map Var args))

     defGFun  f (x : b : args) =<< doIf (xe `lt` tgt') body (Pure be)
     pure $ Call f (intL 0 sizeType : newBuilder ty : map Var anames)

pSkipAtMost :: Backtrack -> Expr -> Grammar -> M Grammar
pSkipAtMost cmt tgt p =
  do f <- newFName "Many" sizeType
     ((tgt', p'), nameMap) <- rename (tgt, p)
     let (anames, args) = unzip (Map.toList nameMap)
     x <- newLocal sizeType
     let xe = Var x
     skipBody <- maybeSkip cmt p' (Call f (add xe (intL 1 sizeType) : map Var args))
                                  (Pure xe)
     defGFun f (x:args) =<< doIf (xe `lt` tgt') skipBody (Pure xe)
     pure (Call f (intL 0 sizeType : map Var anames))

pParseAtMost :: Backtrack -> Type -> Expr -> Expr -> Grammar -> M Grammar
pParseAtMost cmt ty tgt be p =
  do f <- newFName "Many" (TBuilder ty)
     ((tgt', p'), nameMap) <- rename (tgt, p)
     let (anames, args) = unzip (Map.toList nameMap)
     x <- newLocal sizeType
     bv <- newLocal (TBuilder ty)
     let xe = Var x
         bve = Var bv

     body <- maybeParse cmt ty p'
                (\a -> Call f (add xe (intL 1 sizeType)
                              : emit bve a
                              : map Var args))
                (Pure bve)

     defGFun f (x : bv : args) =<< doIf (xe `lt` tgt') body (Pure bve)
     pure $ Call f (intL 0 sizeType : be : map Var anames)

finishMany :: Type -> Grammar -> M Grammar
finishMany ty p = do
  b <- newLocal (TBuilder ty)
  pure (Do b p (Pure (finishBuilder (Var b))))

-- -----------------------------------------------------------------------------
-- Repeat

noRepeatLoop :: Backtrack -> Name -> Expr -> Grammar -> M Grammar
noRepeatLoop cmt x e p =
  do (p', nameMap) <- renameIgnoring [x] p
     let (anames, args) = unzip (Map.toList nameMap)
     let lab = case cmt of
                 Eager -> "many"
                 Lazy  -> "many?"
     f     <- newFName lab ty
     let def = do lhs <- do r1 <- newLocal ty
                            pure (Do r1 p' (Pure (just (Var r1))))
                  r2 <- newLocal (TMaybe ty)
                  pure $ Do r2 (orOp cmt lhs (Pure (nothing ty)))
                       $ GCase
                       $ Case r2
                           [ (PJust, Call f (eFromJust (Var r2) : map Var args))
                           , (PNothing, Pure (Var x))
                           ]
     defGFun f (x : args) =<< def
     pure (Call f (e : map Var anames))
  where
    ty = typeOf p

-- -----------------------------------------------------------------------------
-- Morphisms

noLoopMorphism :: (TypeOf a, CoreSyn a, Rename a) =>
  LoopMorphism a ->
  (FName -> [Name] -> a -> M ()) ->
  (Expr -> a) ->
  (Name -> a -> a -> a) ->
  M a
noLoopMorphism lm defFun inj bindf =
  case lm of
    FoldMorphism x e lc b -> do
      (b', nameMap) <- renameIgnoring (x : loopCollectionBinders lc) b
      let (anames, args) = unzip (Map.toList nameMap)
          ty = typeOf e
          colT = typeOf (lcCol lc)
          maybeAddKey e'
            | Just k <- lcKName lc = coreLet k e'
            | otherwise = id

      f <- newFName "fold" ty
      i <- newLocal (TIterator colT)
      nextS <- newLocal ty
      defFun f (x : i : args)
       =<< doIf (iteratorDone (Var i))
              (inj (Var x))
              (coreLet (lcElName lc) (iteratorVal (Var i))
               $ maybeAddKey (iteratorKey (Var i))
               $ bindf nextS b'
                   (coreCall f (Var nextS : iteratorNext (Var i) : map Var args)))
      pure $ coreCall f (e : newIterator (lcCol lc) : map Var anames)

    MapMorphism lc b -> do
      (b', nameMap) <- renameIgnoring (loopCollectionBinders lc) b
      let colT = typeOf (lcCol lc)
      i <- newLocal (TIterator colT)

      let (anames, args) = unzip (Map.toList nameMap)
          maybeAddKey e'
            | Just k <- lcKName lc = coreLet k e'
            | otherwise = id
          bTy = typeOf b'
          (rty, mknext, initAcc, mkRes) =
            case colT of
              TArray  _ -> ( TBuilder bTy
                           , emit
                           , newBuilder bTy
                           , \c -> do x <- newLocal (TBuilder bTy)
                                      pure (bindf x c (inj (finishBuilder (Var x))))
                           )
              TMap kT _ -> (TMap kT bTy
                           , \m -> mapInsert m (iteratorKey (Var i))
                           , mapEmpty kT bTy
                           , pure
                           )
              _ -> panic "noLoopMorphism" ["Unexpected result type"]

      f <- newFName "map" rty

      acc <- newLocal rty
      newEl <- newLocal bTy
      defFun f (acc : i : args)
       =<< doIf (iteratorDone (Var i))
              (inj (Var acc))
              (coreLet (lcElName lc) (iteratorVal (Var i))
               $ maybeAddKey (iteratorKey (Var i))
               $ bindf newEl b'
                   (coreCall f (mknext (Var acc) (Var newEl)
                                : iteratorNext (Var i)
                                : map Var args)))

      mkRes $ coreCall f (initAcc : newIterator (lcCol lc) : map Var anames)

