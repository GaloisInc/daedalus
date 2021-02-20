{-# Language GADTs, ViewPatterns, RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}

-- Parser combinators for reading back a model from the solver

module Talos.SymExec.ModelParser
  ( ModelP, evalModelP
  , pBytes, pMUnit, pIndexed, pSeq
  ) where

import qualified Data.Vector as V
import Control.Monad (ap, guard)
import Control.Applicative
import Data.List (foldl')
import Data.Char (isDigit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map(Map)
import qualified Data.Map as Map

import Data.Word
import Text.Read (readMaybe)

import Daedalus.Panic

import SimpleSMT (SExpr(..))
  
--------------------------------------------------------------------------------
-- Model Combinators
--
-- Only what is needed by the relation type generated in SymExec.hs
-- (sum, product, bytes, unit) 

pBytes :: ModelP ByteString
pBytes = pSExpr $ do
  pExact "bytes"
  pByteString

pMUnit :: ModelP ()
pMUnit = pExact "munit"

pIndexed :: (Int -> ModelP a) -> ModelP a
pIndexed rest = pSExpr $ do
  pExact "indexed"
  n <- pNumber
  rest (fromIntegral n)

-- pSeq :: (Int -> ModelP a) -> ModelP a
-- pSeq m = pSExpr $ do
--   pExact "seq"
--   n  <- pNumber -- count
--   pArray n (m (fromIntegral n))

pSeq :: ModelP a -> ModelP b -> ModelP (a, b)
pSeq pl pr = pSExpr $ do
  pExact "seq"
  (,) <$> pl <*> pr
  
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
  empty    = ModelP $ \_ -> []
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

withSExprs :: [SExpr] -> ModelP a -> ModelP a
withSExprs ss m = ModelP $ \st -> do
  r <- evalModelP' m (st { sexps = ss })
  pure (r, st)

pArray :: Integer -> ModelP a -> ModelP a
pArray count p = do
  (els, arr) <- go
  let v = V.replicate (fromInteger count) arr V.// els
  withSExprs (V.toList v) p  
  where
    -- FIXME: maybe use a mutable vector here?
    base :: ModelP ([(Int, SExpr)], SExpr)
    base = pSExpr $ do
      pExact "const"
      s <- pRawHead
      pure ([], s)

    go :: ModelP ([(Int, SExpr)], SExpr)
    go =
      base <|> 
      (pSExpr $
        do pExact "store"
           (els, arr) <- go
           i   <- pNumber
           s   <- pRawHead
           pure (if i < count then (fromInteger i, s) : els else els, arr))

-- FIXME: do we need this?

-- getValue :: String -> ModelP SExpr
-- getValue a = do
--   m_sexpr <- ModelP $ asks (Map.lookup a . vEnv)
--   case m_sexpr of
--     Just v -> pure v
--     Nothing -> do
--       s <- ModelP $ asks solver
--       res <- liftIO $ command s $ fun "get-value" [List [S.const a]]
--       case res of
--         List [List [_, v]] -> pure v
--         _ -> panic (unlines
--                 [ "Unexpected response from the SMT solver:"
--                 , "  Exptected: a value"
--                 , "  Result: " ++ showsSExpr res ""
--                 ]) []


--------------------------------------------------------------------------------
-- Combinators
--
-- Only what is needed by the relation type generated in SymExec.hs
-- (sum, product, bytes, unit) 

-- pTuple :: ModelP a -> ModelP b -> ModelP (a, b)
-- pTuple l r = pSExpr $ do
--   pExact "mk-tuple"
--   (,) <$> l <*> r

-- pUnit :: ModelP ()
-- pUnit = pExact "unit"

-- pSum :: ModelP a -> ModelP a -> ModelP a
-- pSum l r = pSExpr $ do
--   (pExact "inl" *> l) <|> (pExact "inr" *> r)

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

-- This is nicer, we could use pHead directly
pList :: ModelP a -> ModelP [a]
pList p = ([] <$ pExact "nil") <|>
          (pSExpr $ do pExact "insert"
                       (:) <$> p <*> pList p)

pByteString :: ModelP ByteString
pByteString = BS.pack <$> pList pByte
  
--------------------------------------------------------------------------------
-- Helpers

ident :: SExpr -> Maybe String
ident (Atom a) = Just a
ident (List [ident -> Just "as", v, _]) = ident v
ident _ = Nothing

-- --------------------------------------------------------------------------------
-- --------------------------------------------------------------------------------
-- --------------------------------------------------------------------------------
-- --------------------------------------------------------------------------------
-- --------------------------------------------------------------------------------
-- --------------------------------------------------------------------------------
-- --------------------------------------------------------------------------------
-- --------------------------------------------------------------------------------
-- --------------------------------------------------------------------------------



-- getValue :: Solver -> VEnv -> ModelP a -> String -> IO a
-- getValue s e pOne ((Map.!?) e -> Just r) = runModelP pOne s e r
-- getValue s e pOne v = do
--   res <- command s $ fun "get-value" [List [fun v []]]
--   case res of
--     List [List [_, v]] -> runModelP pOne s e v
--     _ -> fail $ unlines
--                 [ "Unexpected response from the SMT solver:"
--                 , "  Exptected: a value"
--                 , "  Result: " ++ showsSExpr res ""
--                 ]

-- pBool :: ModelP Bool
-- pBool = pLisp (ModelP go)
--   where
--     go s env (ident -> Just "true") = pure True
--     go s env (ident -> Just "false") = pure False
--     go s env (ident -> Just v) = getValue s env pBool v
--     go s env e = error "Unparseable bool"

-- pNumber :: ModelP Integer
-- pNumber = pLisp (ModelP go)
--   where
--     go s env (ident -> Just ('#' : 'x' : rest)) = do
--       let str = "0x" ++ rest
--       case readMaybe str of
--         Nothing -> error ("Couldn't read " ++ show str)
--         Just v  -> pure v
--     go s env (ident -> Just ('#' : 'b' : rest)) = do
--       case binLit rest of
--         Nothing -> error ("Couldn't read " ++ show rest)
--         Just v  -> pure v
--     go s env (ident -> Just ds) | all isDigit ds = do
--       case readMaybe ds of
--         Nothing -> error ("Couldn't read " ++ show ds)
--         Just v  -> pure v
--     go s env (ident -> Just v) = getValue s env pNumber v
--     go s env e = error "Unparseable byte"

--     -- copied from simple-smt
--     binLit cs = do ds <- mapM binDigit cs
--                    return $ sum $ zipWith (*) (reverse ds) powers2
--     powers2   = 1 : map (2 *) powers2
--     binDigit '0' = Just 0
--     binDigit '1' = Just 1
--     binDigit _   = Nothing

-- pByte :: ModelP Word8
-- pByte = fromIntegral <$> pNumber

-- pList :: ModelP a -> ModelP [a]
-- pList p = pLisp (ModelP go)
--   where
--     go s env e = 
--       case e of
--         (ident -> Just "nil") -> pure []
--         -- This would be nicer in pLisp, but we can't distinguish
--         -- between new model variables and regular consts.  Maybe
--         -- check for 'foo!n' symbols?
--         (ident -> Just v)     -> getValue s env (pList p) v
--         List [ident -> Just "insert", e', more] ->
--           (:) <$> runModelP p s env e' <*> go s env more

--         other  -> error $ "Not a list" ++ show e

-- pListWithLength :: ModelP a -> ModelP [a]
-- pListWithLength p = pLisp (ModelP go)
--   where
--     go s env e =
--       case e of
--         (ident -> Just v)     -> getValue s env (pListWithLength p) v
--         List [ident -> Just "mk-ListWithLength", l, _] -> runModelP (pList p) s env l
--         other  -> error $ "Not a list with length " ++ show e
      
                  

-- getBytes :: Solver -> SExpr -> IO ByteString
-- getBytes s e = BS.pack <$> runModelP (pListWithLength pByte) s Map.empty e



-- -- -----------------------------------------------------------------------------
-- -- Top level

-- -- callG :: Solver -> Integer -> Name -> Type -> [Type] -> [I.Value] -> IO ByteString
-- -- callG s offset name resT argTs args = inNewScope s $ do
-- --   b0 <- declare s "|bytes before|" (tList tByte)
-- --   o0 <- declare s "|oracle before|" tOracle
-- --   let i0  = S.fun "mk-input" [b0, S.int offset]
-- --       st0 = S.fun "mk-state" [i0, o0]
-- --   rv  <- declare s "res" (symExecTy resT)
-- --   st' <- declare s "|state after|" tState

-- --   let args' = zipWith symExecValue argTs args
-- --       call = S.fun (ruleName name) (args' ++ [st0])

-- --   -- We don't really care about index, only that it is increasing
-- --   -- assert s (eq (S.fun "index" [o0]) (S.tInt 0)
  
-- --   assert s (eq (S.fun "bytes" [S.fun "input" [st']]) (S.as (S.const "nil") (tList tByte)))

-- --   -- assert that calling the parser doesn't fail (we don't care about
-- --   -- the resulting input (maybe?)
-- --   -- assert s (S.not (fun "is-fail" [call]))
-- --   assert s (eq call (fun "success" [st', rv]))
-- --   r <- check s
-- --   case r of
-- --     Sat -> pure ()
-- --     -- FIXME: pretty gross
-- --     Unsat   -> error "Unsat"
-- --     Unknown -> error "Unknown"

-- --   -- v <- S.getExpr s rv
-- --   -- print v

-- --   getByteModel s b0


-- --------------------------------------------------------------------------------
-- -- Constraint satisfaction

-- -- solvePathConstraints :: Solver
-- --                      -> Map (TCName K.Value) I.Value
-- --                      -> Map (TCName K.Value) VarHandle
-- --                      -> Set VarHandle
-- --                      -> PathConstraints
-- --                      -> IO PathConstraintSolution
-- -- solvePathConstraints s vEnv vMap bsVs pcs = inNewScope s $ do
-- --   -- print pcs
-- --   -- declare all the vars
-- --   mapM_ declVar (Map.toList vMap)
-- --   let env = Env { symVars = (symExecVarHandle <$> vMap) `Map.union` env0 }
-- --   mapM_ declBVar (Set.toList bsVs)
-- --   -- print (show (map (symExecPathConstraint env) pcs))
-- --   mapM_ (S.assert s . symExecPathConstraint env) pcs
-- --   r <- check s
-- --   case r of
-- --     Sat -> pure ()
-- --     -- FIXME: pretty gross
-- --     Unsat   -> error "Unsat"
-- --     Unknown -> error "Unknown"

-- --   rvMap <- Map.fromList <$> mapM getValue' (Map.toList vMap)
-- --   bsMap <- sequence (Map.fromSet (getBytes s . symExecVarHandle) bsVs)
  
-- --   pure (rvMap, bsMap) -- FIXME
-- --   where
-- --     env0               = Map.mapWithKey (\tcn -> symExecValue (tcType tcn)) vEnv
-- --     declVar (tcn, vId) = S.declare s (varHandleName vId)  (symExecTy (tcType tcn))
-- --     declBVar bsId      = S.declare s (varHandleName bsId) (tListWithLength tByte)

-- --     getValue' (tcn, vId) = do
-- --       v <- runModelP (sexprToValue (tcType tcn)) s Map.empty (symExecVarHandle vId)
-- --       -- print $ pp tcn <+> pp v
-- --       pure (vId, v)

-- -- Turns a path constraint into a predicate.
-- -- symExecPathConstraint :: Env -> PathConstraint -> SExpr
-- -- symExecPathConstraint e (SimpleConstraint bsId vId tc) =
-- --   case texprValue tc of
-- --     -- These are the 'simple' terms identified by PathSet: all other terms are handled elsewhere
-- --     TCMatchBytes _ws v ->
-- --       S.and (S.eq (symExecVarHandle bsId) (symExecVarHandle vId))
-- --             (S.eq (symExecVarHandle vId)  (symExecV v))
      
-- --     TCPure v        ->
-- --       S.and (S.eq (symExecVarHandle bsId) (sNil tByte))
-- --             (S.eq (symExecVarHandle vId)  (symExecV v))
      
-- --     TCGetByte {}    -> getByte
-- --     TCMatch _ p    ->
-- --       S.and getByte
-- --             (symExecP p (symExecVarHandle vId))
      
-- --     _               -> error ("BUG: saw a non-simple term: " ++ show (pp tc))
-- --     where
-- --       getByte = S.eq (symExecVarHandle bsId) (sCons (symExecVarHandle vId) (sNil tByte))
      
-- -- symExecPathConstraint e (AssertionConstraint assn) =
-- --   case assn of
-- --     GuardAssertion g -> symExecV g

-- symExecValue :: Type -> I.Value ->  SExpr
-- symExecValue typ val =
--   case val of
--     VUInt n v  -> S.value (S.Bits n v)
--     VSInt n v  -> S.value (S.Bits n v)
--     VInteger v -> S.value (S.Int    v)
--     VBool b    -> S.value (S.Bool b)
--     VUnionElem {} -> unimplemented
--     VStruct    {} -> unimplemented
--     VArray     vs | Just t <- isArray typ ->
--       mkList (map (symExecValue t) (Vector.toList vs))
--     VArray     {} -> error "BUG"
--     VMaybe v | Just t <- isMaybe typ ->
--       case v of
--         Nothing -> sNothing (symExecTy t)
--         Just v  -> sJust (symExecValue t v)
--     VMaybe _   -> error "BUG"
--     VMap {}    -> unimplemented
--     VStream {} -> unimplemented
--   where
--     unimplemented = error $ "symExecValue: Unimplemented " ++ show val
--     mkList :: [SExpr] -> SExpr
--     styp = symExecTy typ
--     mkList xs = fun "mk-ListWithLength"
--                 [ foldr (\a b -> fun "insert" [ a, b ]) (S.as (S.const "nil") (tList styp)) xs
--                 , S.int (fromIntegral $ length xs)
--                 ]

-- -- Constructs a parser for the value of the given type
-- sexprToValue :: Type -> ModelP Value
-- sexprToValue = go Map.empty
--   where
--     -- c.f. symExecTy
--     go env ty =
--       case ty of
--         TVar x | Just t <- env Map.!? x -> go env t
--         TVar x -> error ("Unknown type variable " ++ show (pp x))

--         TCon tname args -> unimplemented -- S.fun (symExecTCTyName tname) (map symExecTy args)
--         Type t ->
--           case t of
--             TGrammar {} -> error "Shouldn't happen (symExecTy: TGrammar)"
--             TStream     -> error "Unimplemented"
--             TByteClass  -> error "Unimplemented" -- Shouldn't happen?
--             TNum {}     -> error "Shouldn't happen (symExecTy: TNum)"
--             TUInt (Type (TNum n)) -> VUInt (fromIntegral n) <$> pNumber
--             TUInt {}    -> error "Shouldn't happen (symExecTy: TUInt)"
--             TSInt (Type (TNum n)) -> VSInt (fromIntegral n) <$> pNumber
--             TSInt {}    -> error "Shouldn't happen (symExecTy: TSInt)"
--             TInteger    -> VInteger <$> pNumber
--             TBool       -> VBool <$> pBool
--             TUnit       -> unimplemented
--             TArray t'   -> VArray . Vector.fromList <$> pListWithLength (sexprToValue t')
--             TMaybe t'   -> unimplemented -- VMaybe <$> pMaybe (sexprToValue env t')
--             TMap kt vt  -> unimplemented -- tMap (symExecTy' env kt) (symExecTy' env vt)

--     unimplemented = error $ "sexprToValue: Unimplemented" 

-- -- -----------------------------------------------------------------------------
-- -- Probably belongs elsewhere

