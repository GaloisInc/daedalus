{-# Language BlockArguments, OverloadedStrings #-}
{-# Language MultiParamTypeClasses #-}
{-# Language GeneralizedNewtypeDeriving #-}
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

Note that `Q` may depend on values defined before `P`, so really we
should return all variables that `Q` needs:

> { let y = A; let x = P; Q } <| R

becomes:

> block
>   let mb = { let y = A; let x = P; ^ just (y,x) } <| ^ nothing
>   case mb of
>     just (y,x) > Q
>     nothing -> R

-}
module Daedalus.Core.ShrinkBiasedOr (shrinkModule) where

import qualified Data.Text as Text
import Data.Set(Set)
import qualified Data.Set as Set
import MonadLib

import Daedalus.Panic(panic)
import Daedalus.PP
import Daedalus.GUID
import Daedalus.Rec

import Daedalus.Core
import Daedalus.Core.Free
import Daedalus.Core.Effect(mayFail,mayFailModule)

newtype M a = M { unM :: ReaderT RO (StateT [TDecl] FreshM) a }
  deriving (Functor,Applicative,Monad)

data RO = RO
  { roFailing :: Set FName    -- ^ Function that are known to fail
  , roModName :: MName        -- ^ Current module name
  }

instance BaseM M FreshM where
  inBase m = M $ lift $ lift m

instance HasGUID M where
  guidState f = inBase (guidState f)


shrinkModule :: HasGUID m => Set FName -> Module -> m Module
shrinkModule otherFailing m =
  do let ro = RO { roFailing = mayFailModule m otherFailing
                 , roModName = mName m
                 }
     (fs,decls) <- runFreshIn
                 $ runStateT []
                 $ runReaderT ro
                 $ unM
                 $ mapM shrinkFun (mGFuns m)
     pure m { mGFuns = fs, mTypes = mTypes m ++ map NonRec decls }

shrinkFun :: Fun Grammar -> M (Fun Grammar)
shrinkFun fu = do
  case fDef fu of
    Def e ->
      do e' <- shrink e
         pure fu { fDef = Def e' }
    _ -> pure fu


shrink :: Grammar -> M Grammar
shrink g =
  case g of

    OrBiased g1 g2 ->
      do g1' <- shrink g1
         failing <- M (roFailing <$> ask)
         let s1  = reverse (toStmts Nothing g1')
             (safe,unsafe) = span (stmtMayNotFail failing) s1
         case safe of
           [] -> OrBiased g1' <$> shrink g2
           _  -> case unsafe of
                   [] -> pure g1'
                   u : us -> refactor g2 u us safe

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



refactor :: Grammar -> Stmt -> [Stmt] -> [Stmt] -> M Grammar
refactor g2 u us safe =
  case unsafeVars of
    [] ->
      do let lhs = fromStmts
                 $ reverse
                 $ Stmt Nothing (Pure (boolL True))
                 : u : us
             rhs = Pure (boolL False)
         x <- freshNameSys TBool
         g2' <- shrink g2
         pure $ Do x (OrBiased lhs rhs)
              $ coreIf x safeG g2'

    xs ->
      do (ty,pack,unpack) <-
            case xs of
              [x] -> pure (nameType x, Var x, Let x . eFromJust . Var)
              _   -> makeTuple

         res <- freshNameSys (TMaybe ty)

         let lhs = fromStmts
                 $ reverse $ Stmt Nothing (Pure (just pack)) : u : us
             rhs = Pure (nothing ty)
         g2' <- shrink g2
         pure $ Do res (OrBiased lhs rhs)
              $ coreCase res
                  [ (PNothing, g2')
                  , (PJust, unpack res safeG)
                  ]
  where
  unsafeVars = Set.toList (Set.intersection
                             (Set.unions (map stmtDefines (u : us)))
                             (Set.unions (map stmtUses safe))
                          )
  safeG      = fromStmts (reverse safe)

  labelNames = [ "field" <> Text.pack (show n) | n <- [ 1 :: Int ..] ]
  makeTuple =
    do let labs = zip labelNames unsafeVars
       guid <- getNextGUID
       mname <- M (roModName <$> ask)
       let tname = TName
                     { tnameId   = guid
                     , tnameText = "Tuple"
                     , tnameMod  = mname
                     , tnameAnon = Nothing
                     , tnameRec  = False
                     , tnameBD   = False
                     , tnameFlav = TFlavStruct (map fst labs)
                     }
       M $ sets_ \decls ->
                 TDecl
                     { tName = tname
                     , tTParamKNumber = []
                     , tTParamKValue = []
                     , tDef = TStruct [ (l, nameType x) | (l,x) <- labs ]
                     } : decls

       let ut = UserType
                  { utName = tname
                  , utNumArgs = []
                  , utTyArgs = []
                  }

           ty = TUser ut
           packStruct = Struct ut [ (l, Var x) | (l,x) <- labs ]

       let getField res (l,x) =
             Let x (selStruct (nameType x) l (eFromJust (Var res)))
       pure (ty, packStruct, \res e -> foldr (getField res) e labs)


stmtMayNotFail :: Set FName -> Stmt -> Bool
stmtMayNotFail failing stmt =
  case stmt of
    StmtLet {}   -> True
    StmtAnnot {} -> True
    Stmt _ g     -> not (mayFail failing g)

data Stmt =
    Stmt (Maybe Name) Grammar
  | StmtLet Name Expr
  | StmtAnnot Annot

stmtDefines :: Stmt -> Set Name
stmtDefines stmt =
  case stmt of
    Stmt mb _     -> maybe Set.empty Set.singleton mb
    StmtLet x _   -> Set.singleton x
    StmtAnnot {}  -> Set.empty

stmtUses :: Stmt -> Set Name
stmtUses stmt =
  case stmt of
    Stmt _ g      -> freeVars g
    StmtLet _ e   -> freeVars e
    StmtAnnot {}  -> Set.empty

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


