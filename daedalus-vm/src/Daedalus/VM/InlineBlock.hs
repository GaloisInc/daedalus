{-# Language BangPatterns #-}
module Daedalus.VM.InlineBlock where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Daedalus.Panic
import Daedalus.VM

inlineBlocks :: VMFun -> VMFun
inlineBlocks fun = fun { vmfBlocks = doInline is0 }
  where
  s0  = S { stays = Set.singleton (vmfEntry fun)
          , inline = Set.empty
          }
  usage = foldr updS s0 $ vmfBlocks fun

  is0 = IS { doneBlocks = Map.empty
           , todoBlocks = vmfBlocks fun
           , redirect = Map.empty
           , inlineable = inline usage
           }

data IS = IS
  { doneBlocks  :: Map Label Block
  , todoBlocks  :: Map Label Block
  , redirect    :: Map Label Label
  , inlineable  :: Set Label
  }


doInline :: IS -> Map Label Block
doInline is =
  case Map.lookupMin (todoBlocks is) of
    Nothing -> doneBlocks is
    Just (_,b) -> doInline (extendBlock b is1)
        where is1 = is { todoBlocks = Map.deleteMin (todoBlocks is) }

getBack :: Label -> IS -> (Block,IS)
getBack l is =
  case Map.lookup l (todoBlocks is) of
    Just b -> (b,is)
    Nothing ->
      case Map.lookup l (doneBlocks is) of
        Just b -> (b, is { doneBlocks = Map.delete l (doneBlocks is) })
        Nothing -> panic "getBack" ["Missing block"]

extendBlock :: Block -> IS -> IS
extendBlock b is
  | Just l <- Map.lookup (blockName b) (redirect is) =
    extendBlock (Map.findWithDefault bad l (doneBlocks is)) is

  | otherwise =
    case blockTerm b of
      Jump (JumpPoint l _) | l `Set.member` inlineable is ->
        let (back,is1) = getBack l is
        in
        is1 { doneBlocks = Map.insert (blockName b)
                                      (mergeBlocks b back)
                                      (doneBlocks is1)
            , redirect = Map.insert l (blockName b) (redirect is1)
            }
      _ -> is { doneBlocks = Map.insert (blockName b) b (doneBlocks is) }
  where
  bad = panic "extendBlock" ["Not done?"]


--------------------------------------------------------------------------------
data S = S
  { stays   :: Set Label      -- ^ This must stay in block form
  , inline  :: Set Label      -- ^ These can be inline (disjoint from stays)
  }

addStays :: JumpPoint -> S -> S
addStays (JumpPoint l _) s =
  S { stays = Set.insert l (stays s)
    , inline = Set.delete l (inline s)
    }

addInline :: JumpPoint -> S -> S
addInline j@(JumpPoint l _) s
  | l `Set.member` stays s  = s
  | l `Set.member` inline s = addStays j s
  | otherwise               = s { inline = Set.insert l (inline s) }

updS :: Block -> S -> S
updS b =
  case blockTerm b of
    Jump j          -> addInline j
    JumpIf _ j1 j2  -> addStays j1 . addStays j2
    Yield           -> id
    ReturnNo        -> id
    ReturnYes _     -> id
    ReturnPure _    -> id
    Call _ _ x y _  -> addStays x . addStays y
    _               -> id






mergeBlocks :: Block -> Block -> Block
mergeBlocks front back =
  front { blockLocalNum = blockLocalNum front + blockLocalNum back
        , blockInstrs = blockInstrs front ++ map renI (blockInstrs back)
        , blockTerm = renC (blockTerm back)
        }

  where
  su = case blockTerm front of
         Jump (JumpPoint l es)
           | blockName back == l -> Map.fromList (zip (blockArgs back) es)
         _ -> panic "mergeBlocks" ["Blocks don't match"]


  renI i =
    case i of
      CallPrim f e x -> CallPrim f (map renE e) (renBV x)
      GetInput x     -> GetInput (renBV x)
      Spawn j x      -> Spawn (renJ j) (renBV x)

      SetInput e     -> SetInput (renE e)
      Say s          -> Say s
      Output e       -> Output (renE e)
      Notify e       -> Notify (renE e)
      NoteFail       -> NoteFail




  renC c =
    case c of
      Jump j          -> Jump (renJ j)
      JumpIf e j1 j2  -> JumpIf (renE e) (renJ j1) (renJ j2)
      Yield           -> Yield
      ReturnNo        -> ReturnNo
      ReturnYes e     -> ReturnYes (renE e)
      ReturnPure e    -> ReturnPure (renE e)
      Call f c j1 j2 es -> Call f c (renJ j1) (renJ j2) (map renE es)
      TailCall f c es   -> TailCall f c (map renE es)


  renJ (JumpPoint l es) = JumpPoint l (map renE es)

  renBV (BV x t) = BV (x + blockLocalNum front) t

  renE e =
    case e of
      EUnit         -> e
      ENum {}       -> e
      EBool {}      -> e
      ENothing {}   -> e
      EMapEmpty {}  -> e
      EByteArray {} -> e

      EBlockArg ba -> su Map.! ba
      EVar bv      -> EVar (renBV bv)

