{-# Language BlockArguments #-}
module Daedalus.VM.Compile.Monad where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Void(Void)
import Control.Monad(liftM,ap)
import Data.Text(Text)

import Daedalus.PP
import Daedalus.Panic

import qualified Daedalus.Core as Src
import Daedalus.VM
import Daedalus.VM.BlockBuilder



-- | The compiler monad
newtype C a = C (StaticR -> StaticS -> (a,StaticS))


data StaticR = StaticR
  { curFun  :: Text               -- ^ for generating more readable label/names
  , vEnv    :: Map Src.Name FV    -- ^ Compiled expressions
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
  Text ->
  Src.Type ->
  C (BlockBuilder Void) ->
  (Label, Map Label Block)

runC f ty (C m) =
  let (b,info) = m   StaticR { curFun = f
                             , vEnv   = Map.empty
                             , curTy  = TSem ty
                             }
                     StaticS { cLabel = 0
                             , cLocal = 0
                             , cLabels = Map.empty
                             }
      l = Label f (cLabel info)
      (bl,extraArgs) = buildBlock l [] \ ~[] -> b
  in case extraArgs of
       [] -> (l, Map.insert l bl (cLabels info))
       _  -> panic "runC" ["Undefined locals?"]


staticR :: (StaticR -> a) -> C a
staticR f = C \r s -> (f r, s)

getCurTy :: C VMT
getCurTy = staticR curTy

setCurTy :: VMT -> C a -> C a
setCurTy t (C m) = C \r s -> m r { curTy = t } s


lookupN :: Src.Name -> C (BlockBuilder E)
lookupN n =
  do env <- staticR vEnv
     case Map.lookup n env of
       Just b  -> pure (getLocal b)
       Nothing -> panic "lookupN" [ "Undefined name: " ++ show (pp n) ]


gdef :: Src.Name -> FV -> C a -> C a
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
              EBlockArg (BA n _ _) -> Just n
              _ -> Nothing

newLocal :: VMT -> C FV
newLocal t = C \_ s -> let x = cLocal s
                           v = FV x t
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
label1' :: Maybe VMT -> (E -> BlockBuilder Void) -> C (BlockBuilder JumpPoint)
label1' mb def =
  do t <- case mb of
            Nothing -> getCurTy
            Just ty -> pure ty
     (l,vs) <- newBlock [t] \ ~[x] -> def x
     pure do es <- mapM getLocal vs
             pure (JumpPoint l es)







