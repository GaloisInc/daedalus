{-# Language BlockArguments, ConstraintKinds, OverloadedStrings #-}
module Daedalus.Core.X (suceedsMod) where

import Daedalus.GUID
import Daedalus.Panic(panic)
import Daedalus.PP

import Daedalus.Core hiding (freshName)
import Daedalus.Core.Type(typeOf)



data Prop = Expr :=: Expr
          | Expr :/=: Expr
          | Prop :/\: Prop
          | Prop :\/: Prop
          | PFalse
          | PTrue
          | Succeeds FName [Expr] Expr Expr Expr Bool
          | PCase (Case Prop)
          | Forall Name Prop
          | Exists Name Prop
          | PLet Name Expr Prop

pNot :: Prop -> Prop
pNot prop =
  case prop of
    e1 :=: e2   -> e1 :/=: e2
    e1 :/=: e2  -> e1 :=: e2
    p1 :/\: p2  -> pNot p1 :\/: pNot p2
    p1 :\/: p2  -> pNot p1 :/\: pNot p2
    PFalse      -> PTrue
    PTrue       -> PFalse
    Succeeds f es a b c y -> Succeeds f es a b c (not y)
    Forall x p  -> Exists x (pNot p)
    Exists x p  -> Forall x (pNot p)
    PCase c     -> PCase (fmap pNot c)
    PLet x e p  -> PLet x e (pNot p)

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

splitForall :: Prop -> ([Name], Prop)
splitForall p =
  case p of
    Forall x q -> let (ys,z) = splitForall q
                  in (x:ys,z)
    _          -> ([], p)

splitExists :: Prop -> ([Name], Prop)
splitExists p =
  case p of
    Exists x q -> let (ys,z) = splitExists q
                  in (x:ys,z)
    _          -> ([], p)



instance PP Prop where
  pp prop =
    case prop of
      e1 :=: e2  -> pp e1 <+> "==" <+> pp e2
      e1 :/=: e2 -> pp e1 <+> "/=" <+> pp e2
      _ :/\: _   -> case splitAnd prop of
                      []  -> "true"
                      [p] -> pp p
                      ps  -> "and" $$ nest 2 (vcat (map pp ps))
      _ :\/: _   -> case splitOr prop of
                      []  -> "false"
                      [p] -> pp p
                      ps  -> "or" $$ nest 2 (vcat (map pp ps))
      PFalse     -> "false"
      PTrue      -> "true"

      Forall {}  -> case splitForall prop of
                      (xs,q) -> ("forall" <+> hsep (map pp  xs) <.> ".")
                              $$ nest 2 (pp q)

      Exists {}  -> case splitExists prop of
                      (xs,q) -> ("exists" <+> hsep (map pp  xs) <.> ".")
                              $$ nest 2 (pp q)



      Succeeds f es a b c y ->
        kw <+> pp f <.> parens (commaSep (map pp es))
           <+> ppPrec 1 a <+> ppPrec 1 b <+> ppPrec 1 c
        where kw = if y then "succeeds" else "fails"

      PCase c -> pp c

      PLet x e p -> "let" <+> pp x <+> "=" <+> pp e $$ nest 2 (pp p)


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

fAll :: Ctx m => Type -> (Expr -> m Prop) -> m Prop
fAll t k =
  do x <- freshName t
     Forall x <$> k (Var x)

exists :: Ctx m => Type -> (Expr -> m Prop) -> m Prop
exists t k =
  do x <- freshName t
     Exists x <$> k (Var x)

def :: Ctx m => Name -> Expr -> m Prop -> m Prop
def x e k = PLet x e <$> k

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
      exists TStream     \mid ->
      exists (typeOf g1) \r   ->
        succeeds g1 before mid r /\ succeeds g2 mid after result

    Do x g1 g2 ->
      exists TStream     \mid ->
      exists (typeOf g1) \r ->
      succeeds g1 before mid r /\ def x r (succeeds g2 mid after result)

    Let x e g  -> def x e (succeeds g before after result)

    OrBiased g1 g2 ->
      succeeds g1 before after result \/
      (fails g1 before /\ succeeds g2 before after result)

    OrUnbiased g1 g2 ->
      succeeds g1 before after result \/
      succeeds g2 before after result

    Annot _ g -> succeeds g before after result

    Call f es -> assert (Succeeds f es before after result True)

    GCase c   -> succeedsCase c before after result

fails :: Ctx m => Grammar -> Expr -> m Prop
fails g before =
  fAll TStream \after ->
  fAll (typeOf g) \result ->
  pNot <$> succeeds g before after result



--------------------------------------------------------------------------------
succeedsCase :: Ctx m => Case Grammar -> Expr -> Expr -> Expr -> m Prop
succeedsCase c before after result =
  do as <- mapM alt (casePats c)
     pure (PCase c { casePats = as })
  where
  alt (p,k) =
    do prop <- succeeds k before after result
       pure (p,prop)

