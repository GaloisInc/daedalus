{-# Language BlockArguments #-}
{-# Language EmptyCase #-}
module Daedalus.VM.BlockBuilder where

import Data.Map(Map)
import Data.Maybe(isJust,maybeToList)
import qualified Data.Map as Map
import Data.Void(Void)
import Control.Monad(liftM,ap)

import Daedalus.PP
import Daedalus.VM
import qualified Daedalus.Core.Basics as Src



newtype BlockBuilder a = BlockBuilder ((a -> R) -> R)

type R = BuildInfo -> ([Instr], (CInstr, Int, Maybe BA, [(BA,FV)]))


data BuildInfo = BuildInfo
  { nextLocal   :: Int
  , nextArg     :: Int
  , localDefs   :: Map FV E
  , inputVal    :: Maybe E
  , externInp   :: Maybe BA
  , externArgs  :: [(BA,FV)]
  }

data FV = FV Int VMT
  deriving (Eq,Ord)

instance PP FV where
  pp (FV x t) = parens (text "fv" <.> int x <+> pp t)


instance HasType FV where getType (FV _ t) = t

instance Functor BlockBuilder where
  fmap = liftM

instance Applicative BlockBuilder where
  pure a = BlockBuilder \k -> k a
  (<*>)  = ap

instance Monad BlockBuilder where
  BlockBuilder m >>= f =
    BlockBuilder \k -> m \a ->
                       let BlockBuilder m2 = f a
                       in m2 k

getLocal :: FV -> BlockBuilder E
getLocal x = BlockBuilder \k info ->
                case Map.lookup x (localDefs info) of
                  Just e  -> k e info

                  -- not defined: becaomes a parameter to the block
                  Nothing ->
                    let a = nextArg info
                        arg = BA a (getType x) Borrowed {- placeholder -}
                        e = EBlockArg arg
                        i1 = info { nextArg = a + 1
                                  , localDefs = Map.insert x e (localDefs info)
                                  , externArgs = (arg,x) : externArgs info
                                  }
                    in k e i1


setLocal :: FV -> E -> BlockBuilder ()
setLocal x e = BlockBuilder \k i ->
  let i1 = i { localDefs = Map.insert x e (localDefs i) }
  in k () i1


getInput :: BlockBuilder E
getInput = BlockBuilder \k info ->
             case inputVal info of
               Just e  -> k e info
               Nothing ->
                 let a   = nextArg info
                     arg = BA a (TSem Src.TStream) Borrowed {- placeholder -}
                     e   = EBlockArg arg
                     i1  = info { nextArg = a + 1
                                , inputVal = Just e
                                , externInp = Just arg
                                }
                 in k e i1

setInput :: E -> BlockBuilder ()
setInput v = BlockBuilder \k info -> k () info { inputVal = Just v }



stmt :: VMT -> (BV -> Instr) -> BlockBuilder E
stmt ty s = BlockBuilder \k i ->
              let v = nextLocal i
                  x = BV v ty
                  i1 = i { nextLocal = v + 1 }
                  (is, r) = k (EVar x) i1
              in (s x : is, r)

stmt_ :: Instr -> BlockBuilder ()
stmt_ i = BlockBuilder \k info ->
                            let (is, r) = k () info
                            in (i : is, r)

term :: CInstr -> BlockBuilder Void
term c = BlockBuilder \_ i ->
  ([], (c, nextLocal i, externInp i, reverse (externArgs i)))


buildBlock ::
  Label ->
  BlockType ->
  [VMT] ->
  ([E] -> BlockBuilder Void) ->
  (Block, Bool, [FV])
buildBlock nm bty tys f =
  let args = [ BA n t Borrowed{-placeholder-} | (n,t) <- [0..] `zip` tys ]
      BlockBuilder m = f (map EBlockArg args)
      info = BuildInfo { nextLocal = 0
                       , nextArg = length args
                       , localDefs = Map.empty
                       , inputVal = Nothing
                       , externArgs = []
                       , externInp  = Nothing
                       }
      (is,(c,ln,inp,ls)) = m (\v _ -> case v of {}) info
      (extra,free) = unzip ls
  in ( Block { blockName = nm
             , blockType = bty
             , blockArgs = args ++ maybeToList inp ++ extra
             , blockLocalNum = ln
             , blockInstrs = is
             , blockTerm = c
             }
      , isJust inp
      , free
      )

-- | Jump without an argument
jump :: BlockBuilder JumpPoint -> BlockBuilder Void
jump jpb = do jp <- jpb
              term $ Jump jp

-- | Jump-if with no argument
jumpIf ::
  E ->
  BlockBuilder JumpPoint ->
  BlockBuilder JumpPoint ->
  BlockBuilder Void
jumpIf e l1 l2 =
  do jp1 <- l1
     jp2 <- l2
     term $ JumpIf e $ JumpCase
          $ Map.fromList
              [ (PBool True, jumpNoFree jp1)
              , (PBool False, jumpNoFree jp2)
              ]

jumpCase :: Map Pattern (BlockBuilder JumpPoint) -> E -> BlockBuilder Void
jumpCase bs e =
  do jps <- sequence bs
     term $ JumpIf e $ JumpCase $ jumpNoFree <$> jps

