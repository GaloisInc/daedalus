{-# Language ImplicitParams, BlockArguments, OverloadedStrings #-}
{-| This module implements a transformation that reduces the scope
of backtracking for biased choise. Consider `{ let x = P; Q } <| R`
and we know that `Q` cannot fail.   Then we write this to:

> block
>   let mb = { let x = P; ^ just x } <| ^ nothing
>   case mb of
>     just x  -> Q
>     nothing -> R

The benefit is that the backtracking lasts only for the duration of `P`,
and after we know it's outcome we commit to the continuation.
-}
module Daedalus.Core.ShrinkBiasedOr (shrinkModule) where

import Data.Set(Set)

import Daedalus.Panic(panic)
import Daedalus.PP
import Daedalus.GUID

import Daedalus.Core.Basics
import Daedalus.Core.Expr
import Daedalus.Core.Grammar
import Daedalus.Core.Decl
import Daedalus.Core.Effect(mayFail,mayFailModule)


shrinkModule :: HasGUID m => Set FName -> Module -> m Module
shrinkModule otherFailing m =
  do let ?failing = mayFailModule m otherFailing
     fs <- mapM shrinkFun (mGFuns m)
     pure m { mGFuns = fs }

shrinkFun ::
  (?failing :: Set FName, HasGUID m) => Fun Grammar -> m (Fun Grammar)
shrinkFun fu = do
  case fDef fu of
    Def e ->
      do e' <- shrink e
         pure fu { fDef = Def e' }
    _ -> pure fu


shrink :: (?failing :: Set FName, HasGUID m) => Grammar -> m Grammar
shrink g =
  case g of

    OrBiased g1 g2 ->
      do g1' <- shrink g1
         let s1  = reverse (toStmts Nothing g1')
             (safe,unsafe) = span stmtMayNotFail s1
             safeG = fromStmts (reverse safe)
         case safe of
           [] -> OrBiased g1' <$> shrink g2
           _  -> case unsafe of
                   [] -> pure g1'
                   u : us ->
                     case u of
                       StmtLet {} -> panic "shrink" ["unexpected let"]
                       StmtAnnot {} -> panic "shrink" ["unexpected annot"]

                       Stmt Nothing _ ->
                        do let lhs = fromStmts
                                   $ reverse
                                   $ Stmt Nothing (Pure (boolL True))
                                   : u : us
                               rhs = Pure (boolL False)
                           x <- freshNameSys TBool
                           g2' <- shrink g2
                           pure $ Do x (OrBiased lhs rhs) $ coreIf x safeG g2'

                       Stmt (Just x) _ ->
                        do let lhs = fromStmts
                                   $ reverse
                                   $ Stmt Nothing (Pure (just (Var x)))
                                   : u : us
                               rhs = Pure (nothing (nameType x))
                           y' <- freshName x
                           let y = y' { nameType = TMaybe (nameType x) }
                           g2' <- shrink g2
                           pure $ Do y (OrBiased lhs rhs)
                                $ coreCase y
                                    [ (PNothing, g2')
                                    , (PJust, Let x (eFromJust (Var y)) safeG)
                                    ]

    OrUnbiased g1 g2  -> OrUnbiased <$> shrink g1 <*> shrink g2
    Do_ g1 g2         -> Do_ <$> shrink g1 <*> shrink g2
    Do x g1 g2        -> Do x <$> shrink g1 <*> shrink g2
    Let x e g1        -> Let x e <$> shrink g1
    Annot a g1        -> gAnnotate [a] <$> shrink g1
    GCase c           -> GCase <$> traverse shrink c
    Loop l            -> Loop <$> traverse shrink l

    Pure {}           -> pure g
    Fail {}           -> pure g
    SetStream {}      -> pure g
    GetStream {}      -> pure g
    Call {}           -> pure g
    Match {}          -> pure g

stmtMayNotFail :: (?failing :: Set FName) => Stmt -> Bool
stmtMayNotFail stmt =
  case stmt of
    StmtLet {}   -> True
    StmtAnnot {} -> True
    Stmt _ g     -> not (mayFail ?failing g)

data Stmt =
    Stmt (Maybe Name) Grammar
  | StmtLet Name Expr
  | StmtAnnot Annot

instance PP Stmt where
  pp stmt =
    braces
      case stmt of
        Stmt mb g ->
          case mb of
            Nothing -> pp g
            Just x  -> pp x <+> "<-" <+> pp g
        StmtLet x e -> "let" <+> pp x <+> "=" <+> pp e
        StmtAnnot a -> "annot" <+> pp a

-- Assume no shadowing of locals.
toStmts :: Maybe Name -> Grammar -> [Stmt]
toStmts x g =
  case g of
    Do_ g1 g2   -> toStmts Nothing g1 ++ toStmts x g2
    Do y g1 g2  -> toStmts (Just y) g1 ++ toStmts x g2
    Let a e g1  -> StmtLet a e : toStmts x g1
    Annot a g1  -> StmtAnnot a : toStmts x g1
    _           -> [Stmt x g]

fromStmts :: [Stmt] -> Grammar
fromStmts stmts =
  case stmts of
    [] -> panic "fromStmts" ["[]"]
    [ Stmt Nothing g ] -> g
    [ _ ] -> panic "fromStmts" [ "Unexpected last statement." ]
    stmt : more ->
      case stmt of
        StmtLet x e -> Let x e (fromStmts more)
        Stmt mb g ->
          case mb of
            Nothing -> Do_ g (fromStmts more)
            Just x  -> Do x g (fromStmts more)
        StmtAnnot a -> gAnnotate [a] (fromStmts more)


