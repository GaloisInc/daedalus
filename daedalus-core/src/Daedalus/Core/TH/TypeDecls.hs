{-# Language BlockArguments #-}
{-# Language TemplateHaskell #-}
{-# Language ImplicitParams #-}
module Daedalus.Core.TH.TypeDecls (compileTDecls) where

import qualified Data.Text as Text
import Data.Maybe(mapMaybe)
import qualified Data.Map as Map
import qualified Data.Kind as K
import qualified GHC.TypeNats as K
import qualified GHC.Records as R
import Control.Monad(forM)
import Language.Haskell.TH(Q)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Datatype.TyVarBndr as TH

import qualified Daedalus.RTS as RTS

import Daedalus.Rec(Rec,recToList)
import qualified Daedalus.BDD as BDD
import Daedalus.Core.Basics
import Daedalus.Core.Decl
import Daedalus.Core.TH.Names
import Daedalus.Core.TH.Type


compileTDecls :: [Rec TDecl] -> TH.DecsQ
compileTDecls rs = concat <$> traverse compileTDeclRec rs

compileTDeclRec :: Rec TDecl -> TH.DecsQ
compileTDeclRec decls =
  do let ds = recToList decls
     concat <$> traverse compileTDecl ds

compileTDecl :: TDecl -> TH.DecsQ
compileTDecl decl =
  do (nBs,nTs) <- unzip <$> mapM (newTParam [t| K.Nat  |]) (tTParamKNumber decl)
     (vBs,vTs) <- unzip <$> mapM (newTParam [t| K.Type |]) (tTParamKValue decl)
     let ?typeParams = Map.fromList (nTs ++ vTs)
     compileTDef (tName decl) nBs vBs (tDef decl)

newTParam :: Q TH.Kind -> TParam -> Q (TH.TyVarBndr, (TParam, Q TH.Type))
newTParam k p =
  do name <- TH.newName "a"
     v    <- TH.kindedTV name <$> k
     pure (v, (p, pure (TH.VarT name)))


compileTDef ::
  HasTypeParams => TName -> [TH.TyVarBndr] -> [TH.TyVarBndr] -> TDef -> TH.DecsQ
compileTDef name asN asT def =
  case def of
    TStruct fs   -> compileStruct name allParams fs
    TUnion  fs   -> compileUnion  name allParams fs
    TBitdata u d -> compileBitdata name u d
  where
  allParams = asN ++ asT



mkT :: TH.Name -> [TH.TyVarBndr] -> TH.Type
mkT tname as = foldl TH.AppT (TH.ConT tname) (map (TH.VarT . TH.tvName) as)

bangT :: TH.Type -> (TH.Bang, TH.Type)
bangT t = (TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness, t)

standardDeriving :: Q TH.DerivClause
standardDeriving =
  TH.DerivClause Nothing <$> sequence [ [t| Eq |], [t| Ord |], [t| Show |] ]



compileStruct ::
  HasTypeParams => TName -> [TH.TyVarBndr] -> [(Label,Type)] -> TH.DecsQ
compileStruct name as fields =
  do fs <- forM fields \(l,srcT) ->
             case srcT of
               TUnit -> pure (l,Nothing)
               _     -> do bt <- bangT <$> compileType srcT
                           pure (l,Just bt)

     let tname = dataName name
     let ty    = mkT tname as
     let cname = structConName name


     deriv <- standardDeriving
     let con = TH.NormalC cname (mapMaybe snd fs)

     let dataD = case fields of
                   [_] -> TH.NewtypeD [] tname as Nothing  con  [deriv]
                   _   -> TH.DataD    [] tname as Nothing [con] [deriv]


     hasIs <- traverse (hasInstance ty cname fs) fs

     cvtIs <- [d| instance RTS.Convert $(pure ty) $(pure ty) where
                    convert = id |]

     pure (dataD : cvtIs ++ concat hasIs)

  where
  hasInstance ty cname fs (l,mb) =
    do let x     = TH.mkName "x"
           fpats = [ if l == l1 then TH.VarP x else TH.WildP
                   | (l1,Just _) <- fs ]
           lab   = TH.LitT (TH.StrTyLit (Text.unpack l))
           def   = TH.LamE [TH.ConP cname fpats] <$>
                   case mb of
                     Just _  -> pure (TH.VarE x)
                     Nothing -> [| () |]

           ft    = case mb of
                     Nothing    -> [t| () |]
                     Just (_,t) -> pure t

       [d| instance R.HasField $(pure lab) $(pure ty) $ft where
             getField = $def |]


compileUnion ::
  HasTypeParams => TName -> [TH.TyVarBndr] -> [(Label,Type)] -> TH.DecsQ
compileUnion name as cons =
  do fs <- forM cons \(l,srcT) ->
             case srcT of
               TUnit -> pure (l, Nothing)
               _     -> do ty <- compileType srcT
                           pure (l, Just (bangT ty))

     let mkConD (l,mb) = TH.NormalC (unionConName name l)
                         case mb of
                           Nothing ->  []
                           Just t  -> [t]

     deriv <- standardDeriving
     let tname = dataName name
     let dataD = TH.DataD [] tname as Nothing (map mkConD fs) [deriv]
     let ty = mkT tname as
     hasIs <- traverse (hasInstance ty) fs
     cvtIs <- [d| instance RTS.Convert $(pure ty) $(pure ty) where
                    convert = id |]

     pure (dataD : cvtIs ++ concat hasIs)

  where
  lab l = pure (TH.LitT (TH.StrTyLit (Text.unpack l)))

  hasInstance ty (l,mb) =
    let (ft,def) = case mb of
                     Nothing -> ([t| () |], [e| const () |])
                     Just (_,t) ->
                       ( pure t
                       , do x <- TH.newName "x"
                            TH.lamE [TH.conP (unionConName name l) [TH.varP x]]
                                    (TH.varE x)
                       )


    in [d| instance R.HasField $(lab l) $(pure ty) $ft where
             getField = $def
        |]


compileBitdata :: HasTypeParams => TName -> BDD.Pat -> BitdataDef -> TH.DecsQ
compileBitdata name univ def =
  do let tname = dataName name
     let ty    = mkT tname []
     let cname = structConName name

     deriv <- standardDeriving    -- Hm, equality in the presence of wildcards?
     let w = toInteger (BDD.width univ)
     repT  <- compileMonoType (tWord w)
     let con   = TH.NormalC cname [bangT repT]
     let dataD = TH.NewtypeD [] tname [] Nothing con [deriv]

     cvtIs <- [d| instance RTS.Convert $(pure ty) $(pure ty) where
                    convert = id |]
     bdInst <- [d| instance RTS.Bitdata $(pure ty) where
                      type instance BDWidth $(pure ty) =
                                            $(TH.litT (TH.numTyLit w))
                      fromBits = $(TH.conE cname)
                      toBits $(TH.conP cname [[p| x |]]) = x
                |]

     hasIs <- case def of
                BDStruct fs -> traverse (hasInstanceStruct ty cname) fs
                BDUnion cs -> traverse (hasInstanceUnion ty) cs

     pure (dataD : cvtIs ++ bdInst ++ concat hasIs)

  where

  hasInstanceStruct ty cname f =
    case bdFieldType f of
      BDWild     -> pure []
      BDTag {}   -> pure []
      BDData l t ->
        do ft <- compileMonoType t

           x  <- TH.newName "x"
           let p     = TH.conP cname [ TH.varP x ]
               lab   = TH.LitT (TH.StrTyLit (Text.unpack l))
               amt   = bdOffset f
               wt    = compileMonoType (tWord (toInteger (bdWidth f)))

           [d| instance R.HasField $(pure lab) $(pure ty) $(pure ft) where
                 getField $p = RTS.fromBits (
                                 RTS.convert
                                    ($(TH.varE x) `RTS.shiftr` RTS.UInt amt)
                                 :: $wt) |]


  hasInstanceUnion ty (l,t) =
    do let lty = TH.litT (TH.strTyLit (Text.unpack l))
       ft <- compileMonoType t
       [d| instance R.HasField $lty $(pure ty) $(pure ft) where
              getField = RTS.fromBits . RTS.toBits

           instance RTS.Convert $(pure ft) $(pure ty) where
              convert = fromBits . toBits |]



