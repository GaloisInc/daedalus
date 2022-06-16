{-# Language BlockArguments, ConstraintKinds, OverloadedStrings #-}
module Daedalus.Core.X (suceedsMod) where

import Daedalus.GUID
import Daedalus.Panic(panic)
import Daedalus.PP

import Daedalus.Core hiding (freshName)
import Daedalus.Core.Type(typeOf)



data Prop = Expr :=: Expr
          | Prop :/\: Prop
          | Prop :\/: Prop
          | PFalse
          | PTrue
          | Succeeds FName [Expr] Expr Expr Expr
          | Fails FName [Expr] Expr
          | PCase (Case Prop)

splitAnd :: Prop -> [Prop]
splitAnd p0 = go p0 []
  where
  go prop rest =
    case prop of
      p1 :/\: p2 -> go p1 (go p2 rest)
      PTrue      -> rest
      _          -> prop : rest

splitOr :: Prop -> [Prop]
splitOr p0 = go p0 []
  where
  go prop rest =
    case prop of
      p1 :\/: p2 -> go p1 (go p2 rest)
      PFalse     -> rest
      _          -> prop : rest

instance PP Prop where
  pp prop =
    case prop of
      e1 :=: e2  -> pp e1 <+> "==" <+> pp e2
      _ :/\: _   -> "and" $$ nest 2 (vcat (map pp (splitAnd prop)))
      _ :\/: _   -> "or"  $$ nest 2 (vcat (map pp (splitOr prop)))
      PFalse     -> "false"
      PTrue      -> "true"

      Succeeds f es a b c -> "suceeds" <+> pp f <.> parens (commaSep (map pp es))
                                <+> ppPrec 1 a <+> ppPrec 1 b <+> ppPrec 1 c

      Fails f es a -> "fails" <+> pp f <.> parens (commaSep (map pp es))
                              <+> ppPrec 1 a

      PCase c -> pp c


instance DefKW Prop where
  defKW _ = "prop"

--------------------------------------------------------------------------------
type Ctx m = (HasGUID m)

freshName :: Ctx m => Type -> m Name
freshName t =
  do g <- getNextGUID
     pure Name { nameId = g, nameText = Nothing, nameType = t }

freshExpr :: Ctx m => Type -> m Expr
freshExpr t = Var <$> freshName t

assert :: Ctx m => Prop -> m Prop
assert = pure

(/\) :: Ctx m => m Prop -> m Prop -> m Prop
p1 /\ p2 = (:/\:) <$> p1 <*> p2

(\/) :: Ctx m => m Prop -> m Prop -> m Prop
p1 \/ p2 = (:\/:) <$> p1 <*> p2


--------------------------------------------------------------------------------

data FunProp = FunProp Expr Expr Expr Prop

instance DefKW FunProp where
  defKW _ = "prop"

instance PP FunProp where
  pp (FunProp b a r p) =
    vcat [ "before =" <+> pp b
         , "after  =" <+> pp a
         , "result =" <+> pp r
         , pp p
         ]

suceedsMod :: Ctx m => Module -> m [Fun FunProp]
suceedsMod m = traverse succeedsFun (mGFuns m)

succeedsFun :: Ctx m => Fun Grammar -> m (Fun FunProp)
succeedsFun fu = traverse doBody fu
  where
  doBody g =
    do before <- freshExpr TStream
       after  <- freshExpr TStream
       res    <- freshExpr (typeOf (fName fu))
       p      <- succeeds g before after res
       pure (FunProp before after res p)

succeeds :: Ctx m => Grammar -> Expr -> Expr -> Expr -> m Prop
succeeds gram before after result =
  case gram of

    Pure e      -> assert (result :=: e)      /\ assert (before :=: after)

    GetStream   -> assert (result :=: before) /\ assert (before :=: after)

    SetStream e -> assert (result :=: unit)   /\ assert (after :=: e)

    Match {} -> panic "succeeds" ["Match"]

    Fail {}     -> assert PFalse

    Do_ g1 g2   ->
      do mid <- freshExpr TStream
         r   <- freshExpr (typeOf g1)
         succeeds g1 before mid r /\ succeeds g2 mid after result

    Do x g1 g2 ->
      do mid <- freshExpr TStream
         succeeds g1 before mid (Var x) /\ succeeds g2 mid after result

    Let x e g   -> assert (Var x :=: e) /\ succeeds g before after result

    OrBiased g1 g2 ->
      succeeds g1 before after result \/
      (fails g1 before /\ succeeds g2 before after result)

    OrUnbiased g1 g2 ->
      succeeds g1 before after result \/
      succeeds g2 before after result

    Annot _ g -> succeeds g before after result

    Call f es -> assert (Succeeds f es before after result)

    GCase c   -> succeedsCase c before after result




fails :: Ctx m => Grammar -> Expr -> m Prop
fails gram before =
  case gram of

    Pure {} -> assert PFalse

    GetStream -> assert PFalse

    SetStream {} -> assert PFalse

    Match {} -> panic "fails" ["Match"]

    Fail {} -> assert PTrue

    Do_ g1 g2 ->
      do mid <- freshExpr TStream
         r   <- freshExpr (typeOf g1)
         fails g1 before \/ (succeeds g1 before mid r /\ fails g2 mid)

    Do x g1 g2 ->
      do mid <- freshExpr TStream
         fails g1 before \/ (succeeds g1 before mid (Var x) /\ fails g2 mid)

    Let x e g -> assert (Var x :=: e) /\ fails g before

    OrBiased g1 g2   -> fails g1 before /\ fails g2 before

    OrUnbiased g1 g2 -> fails g1 before /\ fails g2 before

    Annot _ g -> fails g before

    Call f es -> assert (Fails f es before)

    GCase c -> failsCase c before


--------------------------------------------------------------------------------
failsCase :: Ctx m => Case Grammar -> Expr -> m Prop
failsCase c before =
  do as <- mapM alt (casePats c)
     pure (PCase c { casePats = as })
  where
  alt (p,k) =
    do prop <- fails k before
       pure (p,prop)

succeedsCase :: Ctx m => Case Grammar -> Expr -> Expr -> Expr -> m Prop
succeedsCase c before after result =
  do as <- mapM alt (casePats c)
     pure (PCase c { casePats = as })
  where
  alt (p,k) =
    do prop <- succeeds k before after result
       pure (p,prop)

