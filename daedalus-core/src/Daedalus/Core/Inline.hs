{-# Language TupleSections, GeneralizedNewtypeDeriving #-}
{-# Language BlockArguments #-}
module Daedalus.Core.Inline (inlineModule) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Foldable as Foldable
import MonadLib
import Data.List(partition)
import Data.Graph.SCC(stronglyConnComp)
import Data.Graph(SCC(..))

import Daedalus.Panic(panic)
import Daedalus.PP(pp)
import Daedalus.Rec(Rec(..))
import Daedalus.GUID(HasGUID)

import Daedalus.Core.Free(FreeVars(freeFVars))
import Daedalus.Core.Subst

import Daedalus.Core.Decl
import Daedalus.Core.Expr
import Daedalus.Core.ByteSet
import Daedalus.Core.Grammar
import Daedalus.Core.Basics

inlineModule :: HasGUID m => [FName] -> Module -> m Module
inlineModule no = runInlineM (Set.fromList no) . expandModule

data Inlineable = Inlineable
  { inlineE  :: Map FName (Fun Expr)
  , inlineG  :: Map FName (Fun Grammar)
  , inlineB  :: Map FName (Fun ByteSet)
  , noInline :: Set FName
  }

addE :: Fun Expr -> Inlineable -> Inlineable
addE f i = i { inlineE = Map.insert (fName f) f (inlineE i) }

addB :: Fun ByteSet -> Inlineable -> Inlineable
addB f i = i { inlineB = Map.insert (fName f) f ( inlineB i) }

addG :: Fun Grammar -> Inlineable -> Inlineable
addG f i = i { inlineG = Map.insert (fName f) f (inlineG i) }

newtype InlineM m a = InlineM (StateT Inlineable m a)
  deriving (Functor,Applicative,Monad,HasGUID)

runInlineM :: HasGUID m => Set FName -> InlineM m a -> m a
runInlineM no (InlineM m) = fst <$> runStateT s m
  where s = Inlineable { inlineE = Map.empty
                       , inlineG = Map.empty
                       , inlineB = Map.empty
                       , noInline = no }

shouldExpand ::
  Monad m => (Inlineable -> Map FName a) -> FName -> InlineM m (Maybe a)
shouldExpand f this = InlineM (Map.lookup this . f <$> get)

isInlineable :: Monad m => FName -> InlineM m Bool
isInlineable f = InlineM (not . (f `Set.member`) . noInline <$> get)

getNoInline :: Monad m => InlineM m (Set FName)
getNoInline = InlineM (noInline <$> get)

instantiate :: (HasGUID m, Subst e) => Fun e -> [Expr] -> InlineM m e
instantiate f es =
  case fDef f of
    External -> panic "instantiate"
                  [ "Trying to inline a primitve: " ++ show (pp (fName f)) ]
    Def e ->
      do let su = Map.fromList (fParams f `zip` es)
         substitute su e

updateInlineable :: Monad m => (Inlineable -> Inlineable) -> InlineM m ()
updateInlineable f = InlineM (sets_ f)


-- | Do inlining in the given thing
class Expand e where
  expand :: HasGUID m => e -> InlineM m e

instance Expand Expr where
  expand expr =
    case expr of
      Var {} -> pure expr
      PureLet x e1 e2 -> PureLet x <$> expand e1 <*> expand e2
      Struct t fs -> Struct t <$> forM fs \(f,e) -> (f,) <$> expand e
      ECase c -> ECase <$> expand c

      Ap0 {} -> pure expr
      Ap1 op e -> Ap1 op <$> expand e
      Ap2 op e1 e2 -> Ap2 op <$> expand e1 <*> expand e2
      Ap3 op e1 e2 e3 -> Ap3 op <$> expand e1 <*> expand e2 <*> expand e3
      ApN op es ->
        do es' <- traverse expand es
           case op of
             CallF f ->
               do mb <- shouldExpand inlineE f
                  case mb of
                    Nothing  -> pure (ApN op es')
                    Just def -> instantiate def es'
             _ -> pure (ApN op es')

instance Expand e => Expand (Case e) where
  expand = traverse expand

instance Expand Grammar where
  expand gram =
    case gram of
      Pure e -> Pure <$> expand e
      GetStream -> pure gram
      SetStream e -> SetStream <$> expand e
      Match s e -> Match s <$> expand e
      Fail e t mb -> Fail e t <$> traverse expand mb
      Do_ g1 g2 -> Do_ <$> expand g1 <*> expand g2
      Do  x g1 g2 -> Do x <$> expand g1 <*> expand g2
      Let x e g -> Let x <$> expand e <*> expand g
      OrBiased g1 g2 -> OrBiased <$> expand g1 <*> expand g2
      OrUnbiased g1 g2 -> OrUnbiased <$> expand g1 <*> expand g2
      Call f es ->
        do es' <- traverse expand es
           mb <- shouldExpand inlineG f
           case mb of
             Nothing  -> pure (Call f es')
             Just yes -> instantiate yes es'
      Annot a g -> Annot a <$> expand g
      GCase c   -> GCase <$> expand c

instance Expand Match where
  expand mat =
    case mat of
      MatchBytes e -> MatchBytes <$> expand e
      MatchByte e  -> MatchByte <$> expand e
      MatchEnd     -> pure MatchEnd

instance Expand ByteSet where
  expand bs =
    case bs of
      SetAny                -> pure bs
      SetSingle e           -> SetSingle <$> expand e
      SetRange e1 e2        -> SetRange <$> expand e1 <*> expand e2
      SetComplement x       -> SetComplement <$> expand x
      SetUnion x y          -> SetUnion <$> expand x <*> expand y
      SetIntersection x y   -> SetIntersection <$> expand x <*> expand y
      SetCase e             -> SetCase <$> expand e
      SetLet x e1 e2        -> SetLet x <$> expand e1 <*> expand e2
      SetCall f es ->
        do es' <- traverse expand es
           mb <- shouldExpand inlineB f
           case mb of
             Nothing  -> pure (SetCall f es')
             Just yes -> instantiate yes es'



instance Expand e => Expand (Fun e) where
  expand f =
    case fDef f of
      Def e ->
        do e' <- expand e
           pure f { fDef = Def e' }
      External -> pure f

--------------------------------------------------------------------------------

-- Inline all: very aggressive
-- XXX: we probably want more configuration here, e.g
--    * hints for what to inline and what not to
--    * whihc definitions we may want to keep, even if they are
--      to be inlined (in case we want to make them into entry points)

orderFuns :: (FreeVars e) => Set FName -> [Fun e] -> [Rec (Fun e)]
orderFuns noI ins = comps ins
  where
  callMap   = Map.fromListWith Set.union
                [ (v,Set.singleton n)
                | (n,vs) <- map deps ins, v <- Set.toList vs
                ]
  callersOf f = Map.findWithDefault Set.empty f callMap

  deps f = (fName f, freeFVars (fDef f))
  comps  = concatMap cvt . stronglyConnComp . map node
  cvt sc = case sc of
             AcyclicSCC n -> [NonRec n]
             CyclicSCC ns -> breakLoop ns

  node a = case deps a of
             (x,xs) -> (a,x,Set.toList xs)

  breakLoop els
    | null els = []
    | otherwise =
    let isNoI fu     = fName fu `Set.member` noI
        isSelfRec fu = fName fu `Set.member` callersOf (fName fu)
    in case (partition isNoI els, partition isSelfRec els) of
         ((b : more,other),_)   -> orderFuns noI (more++other) ++ [MutRec [b]]
         (_, (b : more,other)) -> orderFuns noI (more++other) ++ [MutRec [b]]
         _ -> orderFuns noI (tail els) ++ [MutRec [head els]]


-- we can also inline a recursive function that only tail calls itself
-- but that would have to happen at the VM level...

inlineAll ::
  (HasGUID m, Expand e, FreeVars e) =>
  (Fun e -> Inlineable -> Inlineable) ->
  [Fun e] -> InlineM m [Fun e]
inlineAll ext xs =
  do noI <- getNoInline
     fmap concat $ traverse (inlineRec ext) $ orderFuns noI xs

inlineRec ::
  (HasGUID m, Expand e) =>
  (Fun e -> Inlineable -> Inlineable) ->
  Rec (Fun e) -> InlineM m [Fun e]
inlineRec ext rec =
  do r1 <- traverse expand rec
     case r1 of
       NonRec f | Def {} <- fDef f ->
          do yes <- isInlineable (fName f)
             if yes then updateInlineable (ext f) >> pure []
                    else pure (Foldable.toList r1)

       _ -> pure (Foldable.toList r1)


expandModule :: HasGUID m => Module -> InlineM m Module
expandModule m =
  do efuns <- inlineAll addE (mFFuns m)
     bfuns <- inlineAll addB (mBFuns m)
     gfuns <- inlineAll addG (mGFuns m)
     pure m { mFFuns = efuns, mBFuns = bfuns, mGFuns = gfuns }



