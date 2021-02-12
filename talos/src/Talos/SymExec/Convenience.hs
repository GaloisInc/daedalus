{-# Language TypeFamilies #-}
{-# Language DefaultSignatures #-}
{-# Language UndecidableInstances, TypeOperators #-} -- for SUncurryT default
{-# Language FlexibleInstances, FlexibleContexts #-}

-- A collection of functions making it easier to hand-write SMT lib for e.g. the stdlib
module Talos.SymExec.Convenience where


import Control.Monad.State
import Control.Monad.Reader

import GHC.Generics -- For uncurry and fresh

import SimpleSMT (SExpr, Solver)
import qualified SimpleSMT as S

import Talos.SymExec.Monad

-- -----------------------------------------------------------------------------
-- Defining parsers, functions, etc.

-- defineParser :: SFresh t => Solver -> String -> t -> SExpr -> SFreshT t SParserM
--              -> SolverM SExpr -- (SFreshT t SParserM)
-- defineParser s name args resT f = do
--   (m, args') <- freshFor args f
--   i <- freshSym "input"
--   body <- runSParserM m resT (S.const i)
--   liftIO $ S.defineFun s name (args' ++ [(i, tState)]) (tParserM resT) body

-- defineParserRec :: SFresh t => Solver -> String -> t -> SExpr -> SFreshT t SParserM
--              -> SolverM SExpr -- (SFreshT t SParserM)
-- defineParserRec s name args resT f = do
--   (m, args') <- freshFor args f
--   i <- freshSym "input"
--   body <- runSParserM m resT (S.const i)
--   liftIO $ S.defineFunRec s name (args' ++ [(i, tState)]) (tParserM resT) (\_ -> body)

defineFun :: SFresh t => Solver -> String -> t -> SExpr -> SFreshT t (SolverM SExpr) -> SolverM SExpr
defineFun s name args resT f = do
  (m, args') <- freshFor args f
  body <- m
  liftIO $ S.defineFun s name args' resT body

-- Defines a function for Many P.  More precisely,
-- defineMany s name args pT p creates a parser which parses Many (p args)

-- defineMany :: SFresh t => Solver -> String -> t -> SExpr -> SFreshT t SParserM -> SolverM SExpr
-- defineMany s name args pT p = do
--   (m, args') <- freshFor args p
--   let recargs = map (S.const . fst) args'
--   let body = (sbind (N "x" pT) m
--              (\x  -> sbind (N "xs" (tList pT)) (scallP name recargs)
--              (\xs -> spure (S.fun "insert" [x, xs]))))
--              `salt`
--              (spure (S.fun "as" [S.const "nil", tList S.tInt]))
--   i <- freshSym "input"
--   body' <- runReaderT body (SParserMArgs (tList pT) (S.const i))
--   liftIO $ S.defineFunRec s name (args' ++ [(i, tInput)]) (tParserM (tList pT)) (\_ -> body')

-- This performs better than a version using sAlt
-- defineMany :: SFresh t => Solver -> String -> t -> SExpr -> SFreshT t SParserM -> SolverM SExpr
-- defineMany s name args pT p = do
--   (m, args') <- freshFor args p
--   let recargs = map (S.const . fst) args'
--   i <- freshSym "input"
--   body' <- runReaderT (body m recargs) (SParserMArgs (tList pT) (S.const i))
--   liftIO $ S.defineFunRec s name (args' ++ [(i, tInput)]) (tParserM (tList pT)) (\_ -> body')
--   where
--     body m recargs = do
--       m' <- local (\i -> i { sType = pT }) m
--       r <- S.const <$> freshSym "r"
--       rhs <- S.ite (S.fun "is-fail" [r])
--                <$> (spure (S.as (S.symbol "nil") (tList S.tInt)))
--                <*> (let x = S.fun "result" [r]
--                     in local (\i -> i { sInput = S.fun "input" [r] })
--                              (sbind (N "xs" (tList pT))
--                                     (scallP name recargs)
--                                     (\xs -> spure (S.fun "insert" [x, xs]))))
--       pure (mklet r m' rhs)



-- -----------------------------------------------------------------------------
-- Helper classes for binders

data SBinder = N { sbinderName :: String, sbinderType :: SExpr }

freshBinder :: MonadState SolverMState m => SBinder -> m String -- Binder
-- freshBinder b = (\n -> b { sbinderName = n }) <$> freshSym (sbinderName b)
freshBinder b = freshSym (sbinderName b)  -- FIXME

-- class Freshen t where
--   freshen :: MonadState SolverMState m => t -> m (t, [SBinder])

-- class GFreshen f where
--   gfreshen :: MonadState SolverMState m => f p -> m (f p, [SBinder])

-- instance Freshen () where
--   freshen _ = pure ((), [])

-- instance Freshen SBinder where
--   freshen b = (\a -> (a, [a])) <$> freshBinder b


-- c.f. https://stackoverflow.com/questions/28879421/haskell-is-it-possible-to-create-a-curry-function-that-can-curry-any-number-of
class SUncurry t where
  type SUncurryT t r
  suncurry :: SUncurryT t r -> t -> r
  scurry   :: (t -> r) -> SUncurryT t r

instance (Generic t, GSUncurry (Rep t)) => SUncurry t where
  type SUncurryT t r = GSUncurryT (Rep t) r
  suncurry f    = gsuncurry f . from
  scurry   f    = gscurry   (f . to)

class GSUncurry f where
  type GSUncurryT f r
  gsuncurry :: GSUncurryT f r -> f p -> r
  gscurry   :: (f p -> r)     -> GSUncurryT f r

instance GSUncurry f => GSUncurry (D1 e1 (C1 e2 f)) where
  type GSUncurryT (D1 e1 (C1 e2 f)) r = GSUncurryT f r
  gsuncurry f (M1 (M1 t)) = gsuncurry f t
  gscurry   f             = gscurry (\t -> f (M1 (M1 t)))

instance (GSUncurry a, GSUncurry b) => GSUncurry (a :*: b) where
  type GSUncurryT (a :*: b) r = GSUncurryT a (GSUncurryT b r)
  gsuncurry f (a :*: b) = gsuncurry (gsuncurry f a) b
  gscurry   f           = gscurry (\a -> gscurry (\b -> f (a :*: b)))

instance GSUncurry (S1 e (Rec0 a)) where
  type GSUncurryT (S1 e (Rec0 a)) r = a -> r
  gsuncurry f (M1 (K1 v)) = f v
  gscurry   f         v   = f (M1 (K1 v))

class SFresh a where
  type SFreshT a r
  freshFor :: a -> SFreshT a r -> SolverM (r, [(String, SExpr)])



instance SFresh () where
  type SFreshT () r = r
  freshFor _ f = pure (f, [])

instance SFresh SBinder where
  type SFreshT SBinder r = SExpr -> r
  freshFor b f = do
    v <- freshBinder b
    pure (f (S.const v), [(v, sbinderType b)])

instance SFresh (SBinder, SBinder) where
  type SFreshT (SBinder, SBinder) r = SExpr -> SFreshT SBinder r
  freshFor (a, b) f = do
    v         <- freshBinder a
    (g, args) <- freshFor b (f (S.const v))
    pure (g, (v, sbinderType a) : args)

instance SFresh (SBinder, SBinder, SBinder) where
  type SFreshT (SBinder, SBinder, SBinder) r = SExpr -> SFreshT (SBinder, SBinder) r
  freshFor (a, b, c) f = do
    v         <- freshBinder a
    (g, args) <- freshFor (b, c) (f (S.const v))
    pure (g, (v, sbinderType a) : args)

instance SFresh (SBinder, SBinder, SBinder, SBinder) where
  type SFreshT (SBinder, SBinder, SBinder, SBinder) r = SExpr -> SFreshT (SBinder, SBinder, SBinder) r
  freshFor (a, b, c, d) f = do
    v         <- freshBinder a
    (g, args) <- freshFor (b, c, d) (f (S.const v))
    pure (g, (v, sbinderType a) : args)

instance SFresh (SBinder, SBinder, SBinder, SBinder, SBinder) where
  type SFreshT (SBinder, SBinder, SBinder, SBinder, SBinder) r =
       SExpr -> SFreshT (SBinder, SBinder, SBinder, SBinder) r
  freshFor (a, b, c, d, e) f = do
    v         <- freshBinder a
    (g, args) <- freshFor (b, c, d, e) (f (S.const v))
    pure (g, (v, sbinderType a) : args)

-- instance SFresh (SBinder, SBinder, SBinder, SBinder) where
--   type SFreshT (SBinder, SBinder, SBinder) r = SExpr -> SExpr -> SExpr ->  r
--   freshFor (a, b, c) f = do
--     v         <- freshBinder a
--     (g, args) <- freshFor (b, c) f
--     pure (g v, S.List [v, sbinderType a] : args)
