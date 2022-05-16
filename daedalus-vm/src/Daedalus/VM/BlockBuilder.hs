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

type R = BuildInfo -> ([Instr], BlockEnd)

data BlockEnd = BlockEnd
  { beTerm        :: CInstr
  , beNextLocal   :: Int
  , beExternInp   :: Maybe BA
  , beExternArgs  :: [(BA,FV)]
  }


data BuildInfo = BuildInfo
  { nextLocal   :: Int            -- ^ Used to generate next local var name
  , nextArg     :: Int            -- ^ Used to generate next block argument name
  , localDefs   :: Map FV E       -- ^ Definitions for locally declared variables

  , inputVal    :: Maybe E
    {- ^ The current value of the input stream.
    This starts off as `Nothing` and becomes a block argument if it is
    accessed in this stage. This argument is then stored in `externInp`.

     See 'getInput' and 'setInput'
    -}

  , externInp   :: Maybe BA
    {- ^ Stores the input argument containing the block's input.  If it is
    nothing, then the block does not need external input (i.e., it either
    does not use input or computes the input itself. -}


  , externArgs  :: [(BA,FV)]
    {- These are for "locals" that were used in this block but not defined here
       so they need to be passed in as parameters. -}
  }

-- | Names used while building a block.
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

-- | Access the value of a block variable.
getLocal :: FV -> BlockBuilder E
getLocal x = BlockBuilder \k info ->
                case Map.lookup x (localDefs info) of
                  Just e  -> k e info

                  -- not defined: becomes a parameter to the block
                  Nothing ->
                    let a   = nextArg info
                        arg = BA a (getType x) Borrowed {- placeholder -}
                        e   = EBlockArg arg
                        i1  = info { nextArg = a + 1
                                   , localDefs = Map.insert x e (localDefs info)
                                   , externArgs = (arg,x) : externArgs info
                                   }
                    in k e i1

-- | Define the value for a block variable.
setLocal :: FV -> E -> BlockBuilder ()
setLocal x e = BlockBuilder \k i ->
  let i1 = i { localDefs = Map.insert x e (localDefs i) }
  in k () i1


-- | Access the input stream
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

-- | Set the input instream
setInput :: E -> BlockBuilder ()
setInput v = BlockBuilder \k info -> k () info { inputVal = Just v }


{- | Emit a statement that returns a value of the given type.
The result is an expression that can be used to access the value returned
by the statement. -}
stmt :: VMT -> (BV -> Instr) -> BlockBuilder E
stmt ty s = BlockBuilder \k i ->
              let v = nextLocal i
                  x = BV v ty
                  i1 = i { nextLocal = v + 1 }
                  (is, r) = k (EVar x) i1
              in (s x : is, r)

-- | Emit a statement that does not return a result.
stmt_ :: Instr -> BlockBuilder ()
stmt_ i = BlockBuilder \k info ->
                            let (is, r) = k () info
                            in (i : is, r)

-- | Complete the block by emitting the terminal statement.
term :: CInstr -> BlockBuilder Void
term c = BlockBuilder \_ i ->
  ([], BlockEnd { beTerm        = c
                , beNextLocal   = nextLocal i
                , beExternInp   = externInp i
                , beExternArgs  = reverse (externArgs i)
                })


-- | Build a block.
buildBlock ::
  Label                {- ^ Name for the block -} ->
  BlockType            {- ^ What sort of block it is -} ->
  [VMT]                {- ^ Normal arguments. The actual block may have more -} ->
  ([E] -> BlockBuilder Void)
      {- ^ Use this to jump to this block. Only gets the normal arugments. -} ->
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

      (is,be) = m (\v _ -> case v of {}) info
      (extra,free) = unzip (beExternArgs be)
  in ( Block { blockName = nm
             , blockType = bty
             , blockArgs = args ++ maybeToList (beExternInp be) ++ extra
             , blockLocalNum = beNextLocal be
             , blockInstrs = is
             , blockTerm = beTerm be
             }
      , isJust (beExternInp be)
      , free
      )

-- | Finish a block with a jump at the end.
jump :: BlockBuilder JumpPoint -> BlockBuilder Void
jump jpb = do jp <- jpb
              term $ Jump jp

-- | Finish a block an if (boolean case) at the end.
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

-- | Finsish a block with a case.
jumpCase :: Map Pattern (BlockBuilder JumpPoint) -> E -> BlockBuilder Void
jumpCase bs e =
  do jps <- sequence bs
     term $ JumpIf e $ JumpCase $ jumpNoFree <$> jps

