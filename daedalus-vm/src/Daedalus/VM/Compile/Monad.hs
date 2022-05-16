{-# Language BlockArguments #-}
module Daedalus.VM.Compile.Monad where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Void(Void)
import Control.Monad(liftM,ap,when)
import Data.Text(Text)

import Daedalus.PP
import Daedalus.Panic

import qualified Daedalus.Core as Src
import Daedalus.VM
import Daedalus.VM.BlockBuilder



-- | The compiler monad
newtype C a = C (StaticR -> StaticS -> (a,StaticS))

data DebugMode = DebugStack | NoDebug
  deriving (Eq, Ord, Read, Show)

data StaticR = StaticR
  { curFun    :: Text             -- ^ for generating more readable label/names
  , vEnv      :: Map Src.Name FV  -- ^ Compiled expressions
  , curTy     :: VMT              -- ^ Type of the result we are producing
  , debugMode :: DebugMode        -- ^ Emit debug stack push and pop operations
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
  DebugMode ->
  C (BlockBuilder Void) ->
  (Label, Map Label Block)

runC f ty dm (C m) =
  let (b,info) = m   StaticR { curFun = f
                             , vEnv   = Map.empty
                             , curTy  = TSem ty
                             , debugMode = dm
                             }
                     StaticS { cLabel = 0
                             , cLocal = 0
                             , cLabels = Map.empty
                             }
      l = Label f (cLabel info)
      (bl,inp,extra) = buildBlock l NormalBlock [] \ ~[] -> b
  in case extra of
       [] | not inp -> (l, Map.insert l bl (cLabels info))
       _  -> panic "runC" ["Undefined input/locals?"]


staticR :: (StaticR -> a) -> C a
staticR f = C \r s -> (f r, s)

getCurTy :: C VMT
getCurTy = staticR curTy

setCurTy :: VMT -> C a -> C a
setCurTy t (C m) = C \r s -> m r { curTy = t } s

getDebugging :: C Bool
getDebugging = staticR (\e -> debugMode e == DebugStack)

lookupN :: Src.Name -> C (BlockBuilder E)
lookupN n =
  do env <- staticR vEnv
     case Map.lookup n env of
       Just b  -> pure (getLocal b)
       Nothing -> panic "lookupN" [ "Undefined name: " ++ show (pp n) ]


gdef :: Src.Name -> FV -> C a -> C a
gdef x v (C m) = C \r -> m r { vEnv = Map.insert x v (vEnv r) }


newBlock ::
  BlockType -> [VMT] -> ([E] -> BlockBuilder Void) -> C (Label, Bool, [FV])
newBlock bty tys def = C \r s ->
  let l              = cLabel s
      lab            = Label (curFun r) l
      (b,inp,extra)  = buildBlock lab bty tys def

  in ( (lab, inp, extra)
     , s { cLabel  = l + 1
         , cLabels = Map.insert lab b (cLabels s)
         }
     )  -- jump to jump is handled by InlineBlock later

newLocal :: VMT -> C FV
newLocal t = C \_ s -> let x = cLocal s
                           v = FV x t
                       in ( v
                          , s { cLocal = x + 1 })

label0 :: BlockType -> BlockBuilder Void -> C (BlockBuilder JumpPoint)
label0 bty b =
  do (l,inp,vs) <- newBlock bty []  \ ~[] -> b
     pure do es <- mapM getLocal vs
             is <- if inp then ((:[]) <$> getInput) else pure []
             pure (JumpPoint l (is++es))

label1 ::
  (E -> BlockBuilder Void) ->
  C (E -> BlockBuilder JumpPoint)
label1 def =
  do t <- getCurTy
     (l,inp,vs) <- newBlock NormalBlock [t] \ ~[x] -> def x
     pure \e -> do es <- mapM getLocal vs
                   is <- if inp then (:[]) <$> getInput else pure []
                   pure (JumpPoint l (e:is++es))


-- | For closures
spawnBlock :: (E -> BlockBuilder Void) -> C (BlockBuilder JumpPoint)
spawnBlock def =
  do let t = TSem Src.TBool
     (l,inp,vs) <- newBlock ThreadBlock [t] \ ~[x] -> def x
     when inp $ panic "spawnBlock" [ "Using input?" ]
     pure do es <- mapM getLocal vs
             pure (JumpPoint l es)



-- | For returning from a call to a grammar function
retNo :: BlockBuilder Void -> C (BlockBuilder JumpPoint)
retNo def =
  do (l,inp,vs) <- newBlock (ReturnBlock (RetNo Unknown)) [] \ ~[] -> def
     pure do es <- mapM getLocal vs
             is <- if inp then (:[]) <$> getInput else pure []
             pure (JumpPoint l (is++es))



-- | For returning from a call to a grammar function
retYes :: (E -> BlockBuilder Void) -> C (BlockBuilder JumpPoint)
retYes def =
  do t <- getCurTy
     (l,inp,vs) <- newBlock (ReturnBlock (RetYes Unknown)) [t,TSem Src.TStream]
                                      \ ~[x,i] -> setInput i >> def x
     when inp $ panic "retYes" [ "Input escaped?" ]
     pure do es <- mapM getLocal vs
             -- we don't store the input in the closure because
             -- it will be returned by the function
             pure (JumpPoint l es)


-- | For returning from a call to a grammar function
retPure :: Src.Type -> (E -> BlockBuilder Void) -> C (BlockBuilder JumpPoint)
retPure t def =
  do (l,inp,vs) <- newBlock (ReturnBlock RetPure) [TSem t] \ ~[x] -> def x
     pure do es <- mapM getLocal vs
             is <- if inp then (:[]) <$> getInput else pure []
             pure (JumpPoint l (is++es))


