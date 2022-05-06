{-# Language TemplateHaskell, DataKinds, BlockArguments #-}
{-# Language ImplicitParams #-}
module Daedalus.Core.TH where

import Control.Monad(forM)
import qualified Data.Text as Text
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Kind as K
import qualified GHC.TypeNats as K
import qualified GHC.Records as R

import Language.Haskell.TH (Q)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Datatype.TyVarBndr as TH

import qualified MonadLib as M

import Daedalus.Rec

import qualified RTS.Input    as RTS
import qualified RTS.Numeric  as RTS
import qualified RTS.Vector   as RTS
import qualified RTS.Map      as RTS
import qualified RTS.Iterator as RTS

import Daedalus.Core

compileModule :: Module -> Q [TH.Dec]
compileModule m =
  runCM
  do compileTDecls (mTypes m)

type CM = M.ReaderT Env (M.StateT S Q)

runCM :: CM () -> Q [TH.Dec]
runCM m =
  do let env = Env { vEnv = mempty, tParams = mempty }
         s0  = S { sDecls     = mempty
                 , tEnv       = mempty
                 , tStructCon = mempty
                 , tUnionCons = mempty
                 }
     (_,s) <- M.runStateT s0 (M.runReaderT env m)
     pure (sDecls s)

data S = S
  { sDecls      :: [TH.Dec]  -- generated declaraiotns

    -- Info about data declarations
  , tEnv        :: Map TName  TH.Name
  , tStructCon  :: Map TName  TH.Name
  , tUnionCons  :: Map TName  (Map Label TH.Name)
  }

getTName :: TName -> CM TH.Name
getTName a =
  do s <- M.get
     pure (tEnv s Map.! a)

getStructCon :: TName -> CM TH.Name
getStructCon a =
  do s <- M.get
     pure (tStructCon s Map.! a)

getUnionCons :: TName -> CM (Map Label TH.Name)
getUnionCons a =
  do s <- M.get
     pure (tUnionCons s Map.! a)


addDec :: TH.Dec -> CM ()
addDec d = M.sets_ \s -> s { sDecls = d : sDecls s }

addDecs :: [TH.Dec] -> CM ()
addDecs = mapM_ addDec

addStruct :: TName -> TH.Name -> TH.Name -> CM ()
addStruct t tn cn = M.sets_ \s ->
  s { tEnv       = Map.insert t tn (tEnv s)
    , tStructCon = Map.insert t cn (tStructCon s)
    }

addUnion :: TName -> TH.Name -> [(Label,TH.Name)] -> CM ()
addUnion t tn cns = M.sets_ \s ->
  s { tEnv        = Map.insert t tn (tEnv s)
    , tUnionCons  = Map.insert t (Map.fromList cns) (tUnionCons s)
    }

inQ :: Q a -> CM a
inQ m = M.lift (M.lift m)

newName :: String -> CM TH.Name
newName x = inQ (TH.newName x)

--------------------------------------------------------------------------------
data Env = Env
  { vEnv        :: Map Name   (Q TH.Exp)
  , tParams     :: Map TParam (Q TH.Type)
  }

withV :: Name -> Q TH.Exp -> CM a -> CM a
withV x e = M.mapReader \env ->
  env { vEnv = Map.insert x e (vEnv env) }

withTPS :: [(TParam, Q TH.Type)] -> CM a -> CM a
withTPS xs = M.mapReader \env ->
  env { tParams = Map.union (Map.fromList xs) (tParams env) }



--------------------------------------------------------------------------------

compileTypeCM :: Type -> CM TH.Type
compileTypeCM ty =
  do s <- M.get
     e <- M.ask
     let ?state = s
     let ?env   = e
     inQ (compileType ty)

compileType :: (?state :: S, ?env :: Env) => Type -> Q TH.Type
compileType ty =
  case ty of
    TStream     -> [t| RTS.Input |]
    TUInt sz    -> [t| RTS.UInt $(compileSizeType sz) |]
    TSInt sz    -> [t| RTS.SInt $(compileSizeType sz) |]
    TInteger    -> [t| Integer |]
    TBool       -> [t| Bool |]
    TFloat      -> [t| Float |]
    TDouble     -> [t| Double |]
    TUnit       -> [t| () |]
    TArray t    -> [t| RTS.Vector $(compileType t) |]
    TMaybe t    -> [t| Maybe $(compileType t) |]
    TMap k v    -> [t| RTS.Map $(compileType k) $(compileType v) |]
    TBuilder t  -> [t| RTS.Builder $(compileType t) |]
    TUser ut    -> foldl TH.appT (TH.conT (tEnv ?state Map.! utName ut))
                     $ [ compileSizeType t | t <- utNumArgs ut ]
                    ++ [ compileType     t | t <- utTyArgs ut ]
    TIterator t -> [t| RTS.Iterator $(compileType t) |]
    TParam p    -> tParams ?env Map.! p

compileSizeType :: (?env :: Env) => SizeType -> Q TH.Type
compileSizeType sz =
  case sz of
    TSize n      -> pure (TH.LitT (TH.NumTyLit n))
    TSizeParam p -> tParams ?env Map.! p

--------------------------------------------------------------------------------

compileTDeclName :: TDecl -> CM ()
compileTDeclName tdecl =
  do let srcName = tName tdecl
     let nm      = Text.unpack (tnameText srcName)
     tname <- newName nm
     case tDef tdecl of
       TStruct {} ->
         do cname <- newName nm
            addStruct srcName tname cname
       TUnion ls ->
         do cs <- forM ls \(l,_) ->
                    do cname <- newName (Text.unpack l)
                       pure (l,cname)
            addUnion srcName tname cs

newTParam :: Q TH.Kind -> TParam -> CM (TH.TyVarBndr, (TParam, Q TH.Type))
newTParam k p =
  do name <- newName "a"
     v    <- inQ (TH.kindedTV name <$> k)
     pure (v, (p, pure (TH.VarT name)))



compileTDecl :: TDecl -> CM ()
compileTDecl decl =
  do (nBs,nTs) <- unzip <$> mapM (newTParam [t| K.Nat |])  (tTParamKNumber decl)
     (vBs,vTs) <- unzip <$> mapM (newTParam [t| K.Type |]) (tTParamKValue decl)
     withTPS (nTs ++ vTs) $
       compileTDef (tName decl) (nBs ++ vBs) (tDef decl)

compileTDef :: TName -> [TH.TyVarBndr] -> TDef -> CM ()
compileTDef name as def =
  case def of
    TStruct fs -> compileStruct name as fs

bangT :: TH.Type -> (TH.Bang, TH.Type)
bangT t = (TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, t)

compileStruct :: TName -> [TH.TyVarBndr] -> [(Label,Type)] -> CM ()
compileStruct name as fields0 =
  do fs <- forM fields \(l,srcT) ->
             do bt <- bangT <$> compileTypeCM srcT
                pure (l,bt)

     tname <- getTName name
     let ty = foldl TH.AppT (TH.ConT tname) (map (TH.VarT . TH.tvName) as)

     cname <- getStructCon name


     let deriv = []
         con   = TH.NormalC cname (map snd fs)

     addDec case fields of
              [_] -> TH.NewtypeD [] tname as Nothing  con  deriv
              _   -> TH.DataD    [] tname as Nothing [con] deriv
     mapM_ (hasInstance ty cname) fs

  where
  fields = sorted fields0
  sorted = Map.toList . Map.fromList

  hasInstance ty cname (l,(_,ft)) =
    do let x     = TH.mkName "x"
           fpats = [ if l == l1 then TH.VarP x
                                else TH.WildP | (l1,_) <- sorted fields0 ]
           def   = TH.LamE [TH.ConP cname fpats] (TH.VarE x)
           lab   = TH.LitT (TH.StrTyLit (Text.unpack l))

       addDecs =<<
         inQ
           [d| instance R.HasField $(pure lab) $(pure ty) $(pure ft) where
                  getField = $(pure def)
           |]

compileTDeclRec :: Rec TDecl -> CM ()
compileTDeclRec decls =
  do let ds = recToList decls
     mapM_ compileTDeclName ds
     mapM_ compileTDecl ds

compileTDecls :: [Rec TDecl] -> CM ()
compileTDecls = mapM_ compileTDeclRec



{-
compileExpr :: Env -> Expr -> Q TH.Exp
compileExpr env expr =
  case expr of
    Var x -> vEnv env Map.! x
    PureLet x e1 e2 ->
      [| let y = $(compileExpr env e1)
         in $(compileExpr (withV x [| y |] env) e2)
      |]

-}
