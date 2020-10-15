{-# Language BangPatterns #-}
module Daedalus.VM.InlineBlock where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe(mapMaybe)

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
    JumpIf _ ls     -> stays2 ls
    Yield           -> id
    ReturnNo        -> id
    ReturnYes _     -> id
    ReturnPure _    -> id
    CallPure _ l _  -> addStays l
    Call _ _ ls _   -> stays2 ls
    _               -> id
  where
  stays2 j2 = staysJF (jumpYes j2) . staysJF (jumpNo j2)
  staysJF   = addStays . jumpTarget





mergeBlocks :: Block -> Block -> Block
mergeBlocks front back =
  front { blockLocalNum = blockLocalNum front + blockLocalNum back
        , blockInstrs = blockInstrs front ++ mapMaybe renI (blockInstrs back)
        , blockTerm = renC (blockTerm back)
        }

  where
  su = case blockTerm front of
         Jump (JumpPoint l es)
           | blockName back == l -> Map.fromList (zip (blockArgs back) es)
         _ -> panic "mergeBlocks" ["Blocks don't match"]


  renI i =
    case i of
      CallPrim x f e -> Just $ CallPrim (renBV x) f (map renE e)
      GetInput x     -> Just $ GetInput (renBV x)
      Spawn x j      -> Just $ Spawn (renBV x) (renJ j)

      SetInput e     -> Just $ SetInput (renE e)
      Say s          -> Just $ Say s
      Output e       -> Just $ Output (renE e)
      Notify e       -> Just $ Notify (renE e)
      NoteFail       -> Just $ NoteFail
      Free xs        -> case mapMaybe renV (Set.toList xs) of
                          [] -> Nothing
                          ys -> Just (Free (Set.fromList ys))
      Let x e        -> Just $ Let (renBV x) (renE e)


  renC c =
    case c of
      Jump j          -> Jump (renJ j)
      JumpIf e ls     -> JumpIf (renE e) (renJ2 ls)
      Yield           -> Yield
      ReturnNo        -> ReturnNo
      ReturnYes e     -> ReturnYes (renE e)
      ReturnPure e    -> ReturnPure (renE e)
      Call f ca ls es -> Call f ca (renJ2 ls) (map renE es)
      CallPure f l es -> CallPure f (renJ l) (map renE es)
      TailCall f ca es-> TailCall f ca (map renE es)


  renJ (JumpPoint l es) = JumpPoint l (map renE es)
  renJ2 j2 = JumpChoice
               { jumpYes = renJF (jumpYes j2)
               , jumpNo  = renJF (jumpNo j2)
               }

  renJF jf =
    JumpWithFree
      { freeFirst =  Set.fromList [ x | v <- Set.toList (freeFirst jf)
                                      , Just x <- [renV v]
                                      ]
      , jumpTarget = renJ (jumpTarget jf)
      }

  renBV (BV x t) = BV (x + blockLocalNum front) t

  renV v =
    case v of
      LocalVar bv -> Just $ LocalVar (renBV bv)
      ArgVar ba ->
        case su Map.! ba of
          EBlockArg x -> Just $ ArgVar x
          EVar x      -> Just $ LocalVar x
          _           -> Nothing -- don't need to free constants

  renE e =
    case e of
      EUnit         -> e
      ENum {}       -> e
      EBool {}      -> e
      ENothing {}   -> e
      EMapEmpty {}  -> e

      EBlockArg ba -> su Map.! ba
      EVar bv      -> EVar (renBV bv)

