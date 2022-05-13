{-# Language TemplateHaskell, DataKinds, BlockArguments #-}
{-# Language ImplicitParams #-}
module Daedalus.Core.TH where

import Control.Monad(forM,forM_)
import qualified Data.Text as Text
import Data.Char(isUpper)
import Data.Maybe(mapMaybe)
import Data.Map(Map)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import qualified Data.Kind as K
import qualified GHC.TypeNats as K
import qualified GHC.Records as R

import Language.Haskell.TH (Q)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Datatype.TyVarBndr as TH

import qualified MonadLib as M

import Daedalus.Panic(panic)
import Daedalus.Rec

import qualified RTS          as RTS
import qualified RTS.Input    as RTS
import qualified RTS.Numeric  as RTS
import qualified RTS.Vector   as RTS
import qualified RTS.Map      as RTS
import qualified RTS.Iterator as RTS

import Daedalus.Core.Type(typeOf)
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
                 , tStructConFuns = mempty
                 , tUnionConFuns = mempty
                 }
     (_,s) <- M.runStateT s0 (M.runReaderT env m)
     pure (sDecls s)

data S = S
  { sDecls      :: [TH.Dec]  -- generated declaraiotns

    -- Info about data declarations
  , tEnv        :: Map TName  TH.Name
  , tStructCon  :: Map TName  TH.Name
  , tUnionCons  :: Map TName  (Map Label TH.Name)

  , tStructConFuns :: Map TName StructConFun
  , tUnionConFuns  :: Map TName (Map Label UnionConFun)
  }

type StructConFun = [Q TH.Type] -> [(Label, Q TH.Exp)] -> Q TH.Exp
type UnionConFun  = [Q TH.Type] -> Q TH.Exp -> Q TH.Exp

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

addStructCon :: TName -> StructConFun -> CM ()
addStructCon t f = M.sets_ \s ->
  s { tStructConFuns = Map.insert t f (tStructConFuns s) }

getStructConFun :: (?state :: S) => TName -> StructConFun
getStructConFun t = tStructConFuns ?state Map.! t

addUnionCon :: TName -> Label -> UnionConFun -> CM ()
addUnionCon t l f = M.sets_ \s ->
  s { tUnionConFuns = Map.insertWith Map.union t (Map.singleton l f)
                                                 (tUnionConFuns s) }

getUnionConFun :: (?state :: S) => TName -> Label -> UnionConFun
getUnionConFun t l = (tUnionConFuns ?state Map.! t) Map.! l


inQ :: Q a -> CM a
inQ m = M.lift (M.lift m)

newName :: String -> CM TH.Name
newName x = inQ (TH.newName x)

tdeclIdent :: String -> String
tdeclIdent x =
  case x of
    a : _ -> if isUpper a then x else 'T' : x
    _     -> panic "tdeclIdent" ["Empty name"]

newTDeclName :: String -> CM TH.Name
newTDeclName = pure . TH.mkName . tdeclIdent

newStructConName :: String -> CM TH.Name
newStructConName = pure . TH.mkName . tdeclIdent

newUnionConName :: String -> String -> CM TH.Name
newUnionConName t c = pure (TH.mkName (tdeclIdent t ++ "_" ++ c))


--------------------------------------------------------------------------------
data Env = Env
  { vEnv        :: Map Name   (Q TH.Exp)
  , tParams     :: Map TParam (Q TH.Type)
  }

extV :: Name -> Q TH.Exp -> Env -> Env
extV x e env = env { vEnv = Map.insert x e (vEnv env) }

withV :: Name -> Q TH.Exp -> CM a -> CM a
withV x e = M.mapReader (extV x e)

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
     tname <- newTDeclName nm
     case tDef tdecl of
       TStruct {} ->
         do cname <- newStructConName nm
            addStruct srcName tname cname
       TUnion ls ->
         do cs <- forM ls \(l,_) ->
                    do cname <- newUnionConName nm (Text.unpack l)
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
       compileTDef (tName decl) nBs vBs (tDef decl)

compileTDef :: TName -> [TH.TyVarBndr] -> [TH.TyVarBndr] -> TDef -> CM ()
compileTDef name asN asT def =
  do ddlInstance
     case def of
       TStruct fs -> compileStruct name allParams fs
       TUnion  fs -> compileUnion  name allParams fs

  where
  allParams = asN ++ asT

  ddlInstance =
      do tname <- getTName name
         let ty     = mkT tname allParams
             asmp x = [t| RTS.DDL $(TH.varT (TH.tvName x)) |]
             asmps  = mapM asmp asT

         addDec =<< inQ (TH.instanceD asmps [t| RTS.DDL $(pure ty) |] [])


mkT :: TH.Name -> [TH.TyVarBndr] -> TH.Type
mkT tname as = foldl TH.AppT (TH.ConT tname) (map (TH.VarT . TH.tvName) as)

bangT :: TH.Type -> (TH.Bang, TH.Type)
bangT t = (TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, t)

standardDeriving :: CM TH.DerivClause
standardDeriving =
  TH.DerivClause Nothing <$>
  inQ (sequence [ [t| Eq   |]
                , [t| Ord  |]
                , [t| Show |]
                ])

compileStruct :: TName -> [TH.TyVarBndr] -> [(Label,Type)] -> CM ()
compileStruct name as fields =
  do fs <- forM fields \(l,srcT) ->
             case srcT of
               TUnit -> pure (l,Nothing)
               _     -> do bt <- bangT <$> compileTypeCM srcT
                           pure (l,Just bt)

     tname <- getTName name
     let ty = mkT tname as

     cname <- getStructCon name


     deriv <- standardDeriving
     let con   = TH.NormalC cname (mapMaybe snd fs)

     addDec case fields of
              [_] -> TH.NewtypeD [] tname as Nothing  con  [deriv]
              _   -> TH.DataD    [] tname as Nothing [con] [deriv]


     mapM_ (hasInstance ty cname fs) fs
     addStructCon name (structCon cname fs)

  where
  structCon cname fs ts es =
    let args = [ case lookup l es of
                   Just e  -> e
                   Nothing -> panic "structCon" ["Missing field", show l]
               | (l,Just _) <- fs
               ]
    in TH.appsE (foldl TH.appTypeE (pure (TH.ConE cname)) ts : args)


  hasInstance ty cname fs (l,mb) =
    do let x     = TH.mkName "x"
           fpats = [ if l == l1 then TH.VarP x
                                else TH.WildP
                   | (l1,Just _) <- fs ]
           lab   = TH.LitT (TH.StrTyLit (Text.unpack l))
           def   = TH.LamE [TH.ConP cname fpats] <$>
                   case mb of
                     Just _  -> pure (TH.VarE x)
                     Nothing -> [| () |]

           ft    = case mb of
                     Nothing    -> [t| () |]
                     Just (_,t) -> pure t

       addDecs =<<
         inQ
           [d| instance R.HasField $(pure lab) $(pure ty) $ft where
                  getField = $def
           |]

compileUnion :: TName -> [TH.TyVarBndr] -> [(Label,Type)] -> CM ()
compileUnion name as cons =
  do tname    <- getTName name
     conNames <- do mp <- getUnionCons name
                    pure \l -> mp Map.! l

     fs       <- forM cons \(l,srcT) ->
                   case srcT of
                     TUnit -> pure (l, Nothing)
                     _     -> do ty <- compileTypeCM srcT
                                 pure (l, Just (bangT ty))

     let mkConD (l,mb) = TH.NormalC (conNames l)
                         case mb of
                           Nothing ->  []
                           Just t  -> [t]

     forM_ fs \(l,mb) ->
       addUnionCon name l \ts e ->
          do let c = foldl TH.appTypeE (pure (TH.ConE (conNames l))) ts
             case mb of
               Nothing -> c
               Just _  -> TH.appE c e

     deriv <- standardDeriving
     addDec (TH.DataD [] tname as Nothing (map mkConD fs) [deriv])

     tname <- getTName name
     let ty = mkT tname as
     mapM_ (hasInstance ty conNames) fs

  where
  hasInstance ty conNames (l,mb) =
    do let (ft,def) = case mb of
                        Nothing -> ([t| () |], [e| const () |])
                        Just (_,t) ->
                          ( pure t
                          , do x <- TH.newName "x"
                               TH.lamE [TH.conP (conNames l) [TH.varP x]]
                                       (TH.varE x)
                          )
           lab   = TH.LitT (TH.StrTyLit (Text.unpack l))
       addDecs =<<
         inQ [d| instance R.HasField $(pure lab) $(pure ty) $ft where
                   getField = $def |]


compileTDeclRec :: Rec TDecl -> CM ()
compileTDeclRec decls =
  do let ds = recToList decls
     mapM_ compileTDeclName ds
     mapM_ compileTDecl ds

compileTDecls :: [Rec TDecl] -> CM ()
compileTDecls = mapM_ compileTDeclRec

--------------------------------------------------------------------------------

compileExpr :: (?state :: S) => Env -> Expr -> Q TH.Exp
compileExpr env expr =
  case expr of

    Var x -> vEnv env Map.! x

    PureLet x e1 e2 ->
      [| let y = $(compileExpr env e1)
         in $(compileExpr (extV x [| y |] env) e2)
      |]

    Struct ut fs ->
      let tyArgs = let ?env = env
                   in map compileSizeType (utNumArgs ut) ++
                      map compileType     (utTyArgs ut)

          args = [ (l, compileExpr env e) | (l,e) <- fs ]

      in getStructConFun (utName ut) tyArgs args




compileCase (Case x alts) = undefined
  where
  ty = typeOf x


compilePattern :: Map Label TH.Name -> Pattern -> Q TH.Pat
compilePattern tcons pat =
  case pat of
    PBool b     -> TH.conP (if b then 'True else 'False) []
    PNothing    -> TH.conP 'Nothing []
    PJust       -> TH.conP 'Just [ TH.wildP ]
    PNum n      -> TH.litP (TH.integerL n)
    PBytes bs   -> TH.litP (TH.stringL (BS8.unpack bs))
    PCon l      -> TH.recP (tcons Map.! l) []
    PAny        -> TH.wildP





