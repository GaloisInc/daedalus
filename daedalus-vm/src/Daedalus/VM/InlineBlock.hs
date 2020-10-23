{- | The purpose of this pass is to eliminate jumps by combining blocks.

Consider, for example, these two blocks:

> L1:
>   L1_statements
>   jump L2
>
> L2:
>   L2_statements
>   L2_term

If the jump to @L2@ is the only jump to @L2@, then we can eliminate the
jump and combine the two blocks:

> L1:
>   L1_statements
>   L2_statements
>   L2_term


Another pattern:

> L1(x):
>   jump L2(x)

If @L1@ and @L2@ are the same type of block, we can eliminate @L1@ and
replace occurances of @L1@ with @L2@.

If @L1@ and @L2@ are different types, we can't do the optimization unless
*all* jumps to @L2@ are from indirection blocks like @L1@.  In that case
we can remove the indirections and change the type of @L2:

> L1(x): // return
>   jump L3(x)
>
> L2(x): // return
>   jump L3(x)
>
> L3(x):
>   ...

This would become just:

> L3(x): // return
>   ...

And we replace @L1@ and @L2@ with @L3@.
-}
{-# Language BlockArguments, BangPatterns #-}
{-# Language OverloadedStrings #-}
module Daedalus.VM.InlineBlock where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe(mapMaybe)
import Data.List(group,partition)

import Daedalus.Panic
import Daedalus.VM


-- | We assume that the only block visible to other functions is the entry
inlineBlocks :: VMFun -> VMFun
inlineBlocks fun = jjElim fun { vmfBlocks = doInline is0 }
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
  { stays    :: Set Label       -- ^ This must stay in block form
  , inline   :: Set Label       -- ^ These can be inline (disjoint from stays)
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
    Jump j            -> addInline j
    JumpIf _ ls       -> stays2 ls
    Yield             -> id
    ReturnNo          -> id
    ReturnYes _       -> id
    ReturnPure _      -> id
    CallPure _ l _    -> addStays l
    Call _ _ no yes _ -> addStays no . addStays yes
    TailCall {}       -> id
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
      Call f ca no yes es -> Call f ca (renJ no) (renJ yes) (map renE es)
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


--------------------------------------------------------------------------------
-- Try to eliminate double jumps

jjElim :: VMFun -> VMFun
jjElim fun = fun { vmfBlocks = Map.mapMaybe changeBlock (vmfBlocks fun) }
  where
  jj0   = JJS { jjFixedType = Set.singleton (vmfEntry fun)
              , jjs         = Map.empty
              }

  info  = foldr jjInfo jj0 (Map.elems (vmfBlocks fun))
  subst = foldr (addSu (jjFixedType info)) emptySubst (Map.toList (jjs info))

  labelTy l = blockType (vmfBlocks fun Map.! l)

  addSu fixed (tgt,srcs) su =
    let t = labelTy tgt
        srcs' = [ (s, labelTy s) | s <- srcs ]
        (same,different) = partition ((t ==) . snd) srcs'
        addR l = Map.insert l tgt
        canChange =
          case (t,same, group (map snd srcs')) of
            (NormalBlock,[],[x : _])
              | not (tgt `Set.member` fixed) -> Just x
            _ -> Nothing

    in case canChange of
         Nothing   -> su { jjReplace = foldr addR (jjReplace su)
                                                  (map fst same) }
         Just newT -> su { jjChangeType = Map.insert tgt newT (jjChangeType su)
                         , jjReplace = foldr addR (jjReplace su)
                                                  (map fst different)
                         }

  changeBlock b
    | nm `Map.member` jjReplace subst = Nothing -- remove this block
    | otherwise = Just b { blockType = Map.findWithDefault (blockType b) nm
                                                      (jjChangeType subst)
                         , blockTerm = rewTerm (blockTerm b)
                         }
    where
    nm = blockName b

  rewTerm term =
    case term of
      Jump j            -> Jump (rewJ j)
      JumpIf e jc       -> JumpIf e (rewJC jc)
      Yield             -> term
      ReturnNo          -> term
      ReturnYes {}      -> term
      ReturnPure {}     -> term
      CallPure f l es   -> CallPure f (rewJ l) es
      Call f c l1 l2 es -> Call f c (rewJ l1) (rewJ l2) es
      TailCall {}       -> term

  rewL l  = Map.findWithDefault l l (jjReplace subst)
  rewJ j  = j { jLabel = rewL (jLabel j) }
  rewJC l = JumpChoice { jumpYes = rewJF (jumpYes l)
                       , jumpNo  = rewJF (jumpNo l)
                       }
  rewJF l = l { jumpTarget = rewJ (jumpTarget l) }



data JJSu = JJSu
  { jjChangeType :: Map Label BlockType -- ^ repace type of key
  , jjReplace    :: Map Label Label     -- ^ replace key with value
  } deriving Show

emptySubst :: JJSu
emptySubst = JJSu { jjChangeType = Map.empty, jjReplace = Map.empty }


data JJS = JJS
  { jjFixedType :: Set Label         -- ^ Can't change the type of these blocks
  , jjs         :: Map Label [Label] -- ^ Target to sources
  } deriving Show

jjInfo :: Block -> JJS -> JJS
jjInfo b =
  case blockTerm b of
    Jump l ->
      case blockInstrs b of
        [] | Just as <- mapM isArg (jArgs l)
           , as == blockArgs b
            -> \mp -> mp { jjs = Map.insertWith (++) (jLabel l) [blockName b]
                                                     (jjs mp) }
        _ -> addFixed l

    JumpIf _ ls       -> fixed2 ls
    CallPure _ l _    -> addFixed l
    Call _ _ no yes _ -> addFixed no . addFixed yes
    TailCall {}       -> id
    Yield             -> id
    ReturnNo          -> id
    ReturnYes _       -> id
    ReturnPure _      -> id

  where
  fixed2 j2  = fixedJF (jumpYes j2) . fixedJF (jumpNo j2)
  fixedJF    = addFixed . jumpTarget
  addFixed l = \s -> s { jjFixedType = Set.insert (jLabel l) (jjFixedType s) }

  isArg e    = case e of
                 EBlockArg a -> Just a
                 _           -> Nothing



