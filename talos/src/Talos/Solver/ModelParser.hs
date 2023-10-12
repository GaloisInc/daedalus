{-# Language GADTs, ViewPatterns, RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}

-- Parser combinators for reading back a model from the solver

module Talos.Solver.ModelParser
  ( ModelP, evalModelP
  , pByte, pNumber, pValue, pSExpr, pExact
  ) where

import           Control.Applicative
import           Control.Monad       (ap, guard)
import           Data.Char           (isDigit)
import           Data.List           (foldl')
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Word
import           SimpleSMT           (SExpr (..))
import           Text.Read           (readMaybe)

import           Daedalus.Core
import           Daedalus.Panic      (panic)
import           Daedalus.PP         (showPP)
import qualified Daedalus.Value      as I

-- -----------------------------------------------------------------------------
-- Monadic parser 

-- This is complicated by the fact that the solver returns
-- non-normalised values, including references to other variable, etc.

type VEnv = Map String SExpr

-- This is a bit hacky
data ModelPState =
  ModelPState {  vEnv   :: VEnv
               , sexps  :: [SExpr]
               }

newtype ModelP a = ModelP { runModelP :: ModelPState -> [(a, ModelPState)] }
  deriving (Functor)

instance Applicative ModelP where
  pure v   = ModelP $ \s -> [(v, s)]
  (<*>)    = ap

instance Monad ModelP where
  m >>= f = ModelP $ \s -> do -- list monda
    (a, s') <- runModelP m s
    runModelP (f a) s'
    -- -- Not exactly pretty, but it suffices for out use case
    -- concat <$> mapM (\(a, s') -> runModelP (f a) s') ress

instance Alternative ModelP where
  empty    = ModelP $ const []
  m <|> m' = ModelP $ \s -> runModelP m s ++ runModelP m' s

-- instance MonadIO ModelP where
--   liftIO m = ModelP $ \s -> m >>= \a -> pure [(a, s)]

evalModelP' :: ModelP a -> ModelPState -> [a]
evalModelP' m s = map fst (runModelP m s)

evalModelP :: ModelP a -> SExpr -> [a]
evalModelP m sexpr = evalModelP' m (ModelPState Map.empty [sexpr])

--------------------------------------------------------------------------------
-- Primitive combinators

pRawHead :: ModelP SExpr
pRawHead = ModelP $ \st -> do
  case sexps st of
    []     -> []
    s : ss -> [(s, st { sexps = ss })]

-- Runs the parser p on the first element, taking care of let-related
-- and 'as' annoyances
pHead :: (String -> ModelP a) -> -- ^ This is the atom case, it is in ModelP as it can fail.
         ModelP a -> -- ^ THe list case 
         ModelP a
pHead pA pS = do
  s <- pRawHead
  ModelP $ \st -> do -- list monad
    r <- handleLet st s
    pure (r, st)
  where
    handleLet st (List [ident -> Just "let", List idents, b]) =
      handleLet (st { vEnv = foldl' addBind (vEnv st) idents }) b
    handleLet st (List [ident -> Just "as", s, _]) = handleLet st s
    -- FIXME: This does a map lookup for each atom in the sexp
    handleLet st (Atom a)
      | Just s <- Map.lookup a (vEnv st) = handleLet st s
      | otherwise = evalModelP' (pA a) (st { sexps = [] })
    handleLet st (List ss') = evalModelP' pS (st { sexps = ss' })

    addBind env (List [ident -> Just v, e]) = Map.insert v e env
    addBind _    v = panic "handleLet unexpected let binding" [show v]

pAtom :: ModelP String
pAtom = pHead pure empty

pExact :: String -> ModelP ()
pExact a = pAtom >>= guard . (==) a

pSExpr :: ModelP a -> ModelP a
pSExpr = pHead (const empty)

-- withSExprs :: [SExpr] -> ModelP a -> ModelP a
-- withSExprs ss m = ModelP $ \st -> do
--   r <- evalModelP' m (st { sexps = ss })
--   pure (r, st)

-- pArray :: Integer -> ModelP a -> ModelP a
-- pArray count p = do
--   (els, arr) <- go
--   let v = V.replicate (fromInteger count) arr V.// els
--   withSExprs (V.toList v) p
--   where
--     -- FIXME: maybe use a mutable vector here?
--     base :: ModelP ([(Int, SExpr)], SExpr)
--     base = pSExpr $ do
--       pExact "const"
--       s <- pRawHead
--       pure ([], s)

--     go :: ModelP ([(Int, SExpr)], SExpr)
--     go =
--       base <|>
--       pSExpr (do pExact "store"
--                  (els, arr) <- go
--                  i   <- pNumber
--                  s   <- pRawHead
--                  pure (if i < count then (fromInteger i, s) : els else els, arr))

--------------------------------------------------------------------------------
-- Combinators
--
-- Only what is needed by the relation type generated in SymExec.hs
-- (sum, product, bytes, unit) 

pNumber :: ModelP Integer
pNumber = pAtom >>= go
  where
    go ('#' : 'x' : rest) = do
      let str = "0x" ++ rest
      case readMaybe str of
        Nothing -> error ("Couldn't read " ++ show str)
        Just v  -> pure v
    go ('#' : 'b' : rest) = do
      case binLit rest of
        Nothing -> error ("Couldn't read " ++ show rest)
        Just v  -> pure v
    go ds | all isDigit ds = do
      case readMaybe ds of
        Nothing -> error ("Couldn't read " ++ show ds)
        Just v  -> pure v
    go _ = empty

    -- copied from simple-smt
    binLit cs = do ds <- mapM binDigit cs
                   return $ sum $ zipWith (*) (reverse ds) powers2
    powers2   = 1 : map (2 *) powers2
    binDigit '0' = Just 0
    binDigit '1' = Just 1
    binDigit _   = Nothing

pByte :: ModelP Word8
pByte = fromIntegral <$> pNumber

-- -- This is nicer, we could use pHead directly
-- pList :: ModelP a -> ModelP [a]
-- pList p = ([] <$ pExact "nil") <|>
--           (pSExpr $ do pExact "insert"
--                        (:) <$> p <*> pList p)

-- pByteString :: ModelP ByteString
-- pByteString = BS.pack <$> pList pByte

pBool :: ModelP Bool
pBool = pAtom >>= go
  where
    go "true"  = pure True
    go "false" = pure False
    go _       = empty

-- -----------------------------------------------------------------------------
-- Values

pValue :: Type -> ModelP I.Value
pValue ty0 = go ty0
  where
    -- c.f. symExecTy
    go ty =
      case ty of
        TStream -> unimplemented
        TUInt (TSize n) -> I.vUInt (fromIntegral n) <$> pNumber
        TUInt _ -> unimplemented
        TSInt (TSize n) -> I.vSInt' (fromIntegral n) <$> pNumber
        TSInt _ -> unimplemented
        TInteger -> unimplemented -- FIXME
        TBool -> I.VBool <$> pBool
        TUnit -> pure I.vUnit
        TArray _ -> unimplemented
        TMaybe _ty' -> unimplemented
        TMap {} -> unimplemented
        TBuilder {} -> unimplemented
        TIterator {} -> unimplemented
        TUser _ut -> unimplemented
        TParam {} -> unimplemented
        TFloat    -> unimplemented
        TDouble   -> unimplemented

    unimplemented = panic "sexprToValue: Unimplemented" [ showPP ty0 ]

--------------------------------------------------------------------------------
-- Helpers

ident :: SExpr -> Maybe String
ident (Atom a) = Just a
ident (List [ident -> Just "as", v, _]) = ident v
ident _ = Nothing

