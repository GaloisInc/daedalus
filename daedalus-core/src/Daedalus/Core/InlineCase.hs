{-# Language BlockArguments, GeneralizedNewtypeDeriving #-}
{-|
This module implements the following tranfromation:

> let x = case e of
>           P1 -> { DoStuff; pure A }
>           P2 -> { DoStuff; pure B }
> case x of
>   A -> K1
>   B -> K2

Combine the two cases:

> case e of
>   P1 -> { DoStuff; K1 }
>   P2 -> { DoStuff; K2 }


Limitiations:
  * Note that if the RHS of the first case are repeated,
    it is possible that this duplicates code.
    For the moment we operator on the assumption that this
    is not a problem, but we may want to revisit this.
  * For the moment we only do the rewrite if the cases are
    right next to each other, as in the example above.
    The same should work if there were some intermediate
    statements, as long as we don't cross definitions for
    variables that we need in `K1` and `K2`.
-}
module Daedalus.Core.InlineCase (inlineCaseModule) where

import MonadLib hiding (Label,abort)

import Daedalus.Panic(panic)
import Daedalus.GUID
import Daedalus.Core
import Daedalus.Core.Type

--------------------------------------------------------------------------------

inlineCaseModule :: HasGUID m => Module -> m Module
inlineCaseModule mo =
  do fs <- mapM inlineCaseFun (mGFuns mo)
     pure mo { mGFuns = fs }

inlineCaseFun :: HasGUID m => Fun Grammar -> m (Fun Grammar)
inlineCaseFun fu =
  (\d -> fu { fDef = d}) <$>
  case fDef fu of
    Def e         -> Def <$> inlineCase e
    d@External {} -> pure d

inlineCase :: HasGUID m => Grammar -> m Grammar
inlineCase gram0 =
  do gram <- childrenG inlineCase gram0
     case gram of
        Do x g1 g2
          | Just c2 <- isCase g2
          , caseVar c2 == x
          , Just c1 <- isCase g1 ->
            maybe gram GCase <$> tryCase (rewriteCase x (casePats c2) c1)
        _ -> pure gram

isCase :: Grammar -> Maybe (Case Grammar)
isCase gram =
  case gram of
    Annot _ g -> isCase g
    GCase c   -> Just c
    _         -> Nothing


lookupPattern :: Pattern -> [(Pattern,a)] -> Maybe a
lookupPattern pat ps =
  case ps of
    [] -> Nothing
    (PAny,k) : _ -> Just k
    (p,k) : more
      | p == pat  -> Just k
      | otherwise -> lookupPattern pat more


rewriteCase :: Name -> [(Pattern,Grammar)] -> Case Grammar -> M (Case Grammar)
rewriteCase var conts cas =
  do as <- forM (casePats cas) \(p,g) ->
             do g' <- rewriteCaseAlt var conts g
                pure (p,g')
     pure cas { casePats = as }

-- | Do inlining for this case RHS
-- Assume no name capture
rewriteCaseAlt :: Name -> [(Pattern,Grammar)] -> Grammar -> M Grammar
rewriteCaseAlt var conts gram =
  case gram of
    Pure e            ->
      do (pat,mbVal) <- abortMaybe (isKnownPat e)
         cont        <- abortMaybe (lookupPattern pat conts)
         case mbVal of
           Nothing -> pure cont
           Just res ->
             nameExprIn res \x ->
                pure
                case pat of
                  PJust ->
                    let rew = gebMapChildrenG rew (rewriteFromJust var x) id
                    in rew cont
                  PCon l ->
                    let rew = gebMapChildrenG rew (rewriteFromCon l var x) id
                    in rew cont
                  _      -> panic "rewriteCase" ["Unexpected payload"]

    GetStream         -> abort
    SetStream {}      -> abort
    Match {}          -> abort
    Fail {}           -> abort
    Do_ g1 g2         -> Do_ g1 <$> rewriteCaseAlt var conts g2
    Do x g1 g2        -> Do x g1 <$> rewriteCaseAlt var conts g2
    Let x e g         -> Let x e <$> rewriteCaseAlt var conts g
    OrBiased g1 g2    -> OrBiased <$> rewriteCaseAlt var conts g1
                                  <*> rewriteCaseAlt var conts g2
    OrUnbiased g1 g2  -> OrUnbiased <$> rewriteCaseAlt var conts g1
                                    <*> rewriteCaseAlt var conts g2
    Call {}           -> abort
    Annot a g         -> Annot a <$> rewriteCaseAlt var conts g
    GCase cas         -> GCase <$> rewriteCase var conts cas
    Loop {}           -> abort

-- | Name an expression, unless it is simple.
nameExprIn :: HasGUID m => Expr -> (Expr -> m Grammar) -> m Grammar
nameExprIn e k =
  case e of
    Var {}  -> noName
    Ap0 {}  -> noName
    Ap1 FromJust _ -> noName
    Ap1 (FromUnion _ _) _ -> noName
    _       -> doName

  where
  noName = k e
  doName =
    do x <- freshNameSys (typeOf e)
       Let x e <$> k (Var x)

-- | Replace uses of `fromJust x` with `newVal` in `expr`
rewriteFromJust :: Name -> Expr -> Expr -> Expr
rewriteFromJust x newVal expr =
  case mapChildrenE (rewriteFromJust x newVal) expr of
    Ap1 FromJust (Var y) | x == y -> newVal
    e1 -> e1

-- | Replace uses of `fromJust x` with `newVal` in `expr`
rewriteFromCon :: Label -> Name -> Expr -> Expr -> Expr
rewriteFromCon l x newVal expr =
  case mapChildrenE (rewriteFromCon l x newVal) expr of
    Ap1 (FromUnion _ l1) (Var y) | l == l1 && x == y -> newVal
    e1 -> e1


-- | Is this expression one that will match a pattern directly.
isKnownPat :: Expr -> Maybe (Pattern, Maybe Expr)
isKnownPat expr =
  case expr of
    Ap0 (BoolL l)         -> Just (PBool l, Nothing)
    Ap0 (ENothing _)      -> Just (PNothing, Nothing)
    Ap1 EJust e           -> Just (PJust, Just e)
    Ap0 (IntL n _)        -> Just (PNum n, Nothing)
    Ap0 (ByteArrayL bs)   -> Just (PBytes bs, Nothing)
    Ap1 (InUnion _ l) e   -> Just (PCon l, Just e)
    _                     -> Nothing


--------------------------------------------------------------------------------
-- Monad
newtype M a = M (ExceptionT () FreshM a)
  deriving (Functor,Applicative,Monad)

instance HasGUID M where
  guidState f = M (lift (guidState f))

abort :: M a
abort = M (raise ())

abortMaybe :: Maybe a -> M a
abortMaybe = maybe abort pure

tryCase :: HasGUID m => M a -> m (Maybe a)
tryCase (M m) =
  runFreshIn
  do res <- runExceptionT m
     pure case res of
            Left _  -> Nothing
            Right a -> Just a





