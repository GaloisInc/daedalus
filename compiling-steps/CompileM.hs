{-# Language BlockArguments #-}
module CompileM where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Void(Void)
import Control.Monad(liftM,ap)

import qualified AST
import VM
import qualified Value as Sem
import BlockBuilder




-- | The compiler monad
newtype C a = C (StaticR -> StaticS -> (a,StaticS))


data StaticR = StaticR
  { curFun  :: String             -- ^ for generating more readable label/names
  , vEnv    :: Map AST.N FV       -- ^ Compiled expressions
  , fEnv    :: Map AST.F PrimLab  -- ^ Names for primitives
  , gEnv    :: Map AST.F FunLab   -- ^ Locations of functions
  , curTy   :: VMT                -- ^ Type of the result we are producing
  }

data StaticS = StaticS
  { cLabel    :: Int              -- ^ For generating labels
  , cLocal    :: Int              -- ^ For generating locals
  , cLabels   :: Map Label Block  -- ^ Generated blocks of code
  }

instance Functor C where
  fmap = liftM

instance Applicative C where
  pure a = C \_ s -> (a,s)
  (<*>)  = ap

instance Monad C where
  C m >>= f = C \r s -> let (a,s1) = m r s; C m1 = f a in m1 r s1


runC ::
  AST.F ->
  Map AST.F FunLab ->
  C (BlockBuilder Void) ->
  (Label, Map Label Block)

runC (f,t) env (C m) =
  let (b,info) = m   StaticR { curFun = f
                             , vEnv   = Map.empty
                             , fEnv   = prims
                             , gEnv   = env
                             , curTy  = TSem t
                             }
                     StaticS { cLabel = 0
                             , cLocal = 0
                             , cLabels = Map.empty
                             }
      l = Label f (cLabel info)
      (bl,extraArgs) = buildBlock l [] \ ~[] -> b
  in case extraArgs of
       [] -> (l, Map.insert l bl (cLabels info))
       _  -> error ("Undefined locals?")

  where
  prims = Map.fromList [ (p,PL p) | (p,_) <- Sem.primFuns ]

staticR :: (StaticR -> a) -> C a
staticR f = C \r s -> (f r, s)

getCurTy :: C VMT
getCurTy = staticR curTy

setCurTy :: VMT -> C a -> C a
setCurTy t (C m) = C \r s -> m r { curTy = t } s


computeEff :: AST.P -> C AST.Effect
computeEff p =
  do g <- staticR gEnv
     let getE (FL _ e) = e
     pure (AST.effect (getE <$> g) p)

lookupRule :: AST.F -> C FunLab
lookupRule f =
  do env <- staticR gEnv
     pure case Map.lookup f env of
            Just r -> r
            Nothing -> error ("Undefined rule: " ++ fst f)

lookupN :: AST.N -> C (BlockBuilder E)
lookupN n =
  do env <- staticR vEnv
     case Map.lookup n env of
       Just b  -> pure (getLocal b)
       Nothing -> error ("Undefined name: " ++ fst n)

lookupPrim :: AST.F -> C PrimLab
lookupPrim f =
  do env <- staticR fEnv
     case Map.lookup f env of
       Just fv -> pure fv
       Nothing -> error ("Undefined primitive: " ++ fst f)

gdef :: AST.N -> FV -> C a -> C a
gdef x v (C m) = C \r -> m r { vEnv = Map.insert x v (vEnv r) }


newBlock :: [VMT] -> ([E] -> BlockBuilder Void) -> C (Label, [FV])
newBlock tys def = C \r s ->
  let l               = cLabel s
      lab             = Label (curFun r) l
      (b,extraArgs)   = buildBlock lab tys def
  
  in case isJumpJump b of
      Just otherL -> ( (otherL, extraArgs), s)
      _           -> ( (lab, extraArgs)
                     , s { cLabel  = l + 1
                         , cLabels = Map.insert lab b (cLabels s)
                         }
                     )
  where
  isJumpJump b =
    case (blockInstrs b, blockTerm b) of
      ([], Jump (JumpPoint l es))
        | Just as <- mapM isArg es
        , and (zipWith (==) as [ 0 .. ]) -> Just l
      _ -> Nothing

  isArg e = case e of
              EBlockArg (BA n _) -> Just n
              _ -> Nothing

newLocal :: VMT -> C FV
newLocal t = C \r s -> let x = cLocal s
                           v = FV (curFun r) x t
                       in ( v
                          , s { cLocal = x + 1 })

label0 :: BlockBuilder Void -> C (BlockBuilder JumpPoint)
label0 b = do (l,vs) <- newBlock []  \ ~[] -> b
              pure do es <- mapM getLocal vs
                      pure (JumpPoint l es)

label1 :: (E -> BlockBuilder Void) -> C (E -> BlockBuilder JumpPoint)
label1 def =
  do t <- getCurTy
     (l,vs) <- newBlock [t] \ ~[x] -> def x
     pure \e -> do es <- mapM getLocal vs
                   pure (JumpPoint l (e:es))


-- | For closures
label1' :: (E -> BlockBuilder Void) -> C (BlockBuilder JumpPoint)
label1' def =
  do t <- getCurTy
     (l,vs) <- newBlock [t] \ ~[x] -> def x
     pure do es <- mapM getLocal vs
             pure (JumpPoint l es)







