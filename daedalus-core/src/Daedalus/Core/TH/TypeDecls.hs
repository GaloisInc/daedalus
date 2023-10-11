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

import qualified Daedalus.RTS as RTS

import Daedalus.TH(Q)
import qualified Daedalus.TH as TH
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

newTParam :: Q TH.Kind -> TParam -> Q (TH.DataParam, (TParam, Q TH.Type))
newTParam k p =
  do name <- TH.newName "a"
     v    <- TH.kindedTV name <$> k
     pure (v, (p, TH.varT name))


compileTDef ::
  HasTypeParams => TName -> [TH.DataParam] -> [TH.DataParam] -> TDef -> TH.DecsQ
compileTDef name asN asT def =
  case def of
    TStruct fs   -> compileStruct name allParams fs
    TUnion  fs   -> compileUnion  name allParams fs
    TBitdata u d -> compileBitdata name u d
  where
  allParams = asN ++ asT



mkT :: TH.Name -> [TH.DataParam] -> Q TH.Type
mkT tname as = foldl TH.appT (TH.conT tname) (map (TH.varT . TH.tvName) as)


standardDeriving :: Q TH.DerivClause
standardDeriving = TH.derivClause Nothing [ [t| Eq |], [t| Ord |], [t| Show |] ]



compileStruct ::
  HasTypeParams => TName -> [TH.DataParam] -> [(Label,Type)] -> TH.DecsQ
compileStruct name as fields =
  do let fs = [ case srcT of
                  TUnit -> (l, Nothing)
                  _     -> (l, Just (TH.bangT (compileType srcT)))
              | (l,srcT) <- fields
              ]

     let tname = dataName name
     let ty    = mkT tname as
     let cname = structConName name


     let deriv = standardDeriving
     let con   = TH.normalC cname (mapMaybe snd fs)

     dataD <- case fields of
                [_] -> TH.newtypeD (pure []) tname as Nothing  con  [deriv]
                _   -> TH.dataD    (pure []) tname as Nothing [con] [deriv]


     hasIs <- traverse (hasInstance ty cname fs) fs

     cvtIs <- [d| instance RTS.Convert $ty $ty where
                    convert = id
                    {-# INLINE convert #-}
                |]

     pure (dataD : cvtIs ++ concat hasIs)

  where
  hasInstance ty cname fs (l,mb) =
    let x     = TH.mkName "x"
        fpats = [ if l == l1 then TH.varP x else [p| _ |]
                | (l1,Just _) <- fs ]
        lab   = TH.litT (TH.strTyLit (Text.unpack l))
        def   = TH.lamE [TH.conP cname fpats]
                case mb of
                  Just _  -> TH.varE x
                  Nothing -> [| () |]

        ft    = case mb of
                  Nothing -> [t| () |]
                  Just t  -> snd <$> t
    in
    [d| instance R.HasField $lab $ty $ft where
          getField = $def
          {-# INLINE getField #-}
      |]


compileUnion ::
  HasTypeParams => TName -> [TH.DataParam] -> [(Label,Type)] -> TH.DecsQ
compileUnion name as cons =
  do fs <- forM cons \(l,srcT) ->
             case srcT of
               TUnit -> pure (l, Nothing)
               _     -> do let ty = compileType srcT
                           pure (l, Just (TH.bangT ty))

     let mkConD (l,mb) = TH.normalC (unionConName name l)
                         case mb of
                           Nothing ->  []
                           Just t  -> [t]

     let deriv = standardDeriving
     let tname = dataName name
     dataD <- TH.dataD (pure []) tname as Nothing (map mkConD fs) [deriv]
     let ty = mkT tname as
     hasIs <- traverse (hasInstance ty) fs
     cvtIs <- [d| instance RTS.Convert $ty $ty where
                    convert = id
                    {-# INLINE convert #-}
                |]

     pure (dataD : cvtIs ++ concat hasIs)

  where
  lab l = TH.litT (TH.strTyLit (Text.unpack l))

  hasInstance ty (l,mb) =
    [d| instance R.HasField $(lab l) $ty $ft where
          getField = $def
          {-# INLINE getField #-}
      |]
    where
    (ft,def) = case mb of
                 Nothing -> ([t| () |], [e| const () |])
                 Just t ->
                   ( snd <$> t
                   , do x <- TH.newName "x"
                        TH.lamE [TH.conP (unionConName name l) [TH.varP x]]
                                (TH.varE x)
                   )

compileBitdata :: HasTypeParams => TName -> BDD.Pat -> BitdataDef -> TH.DecsQ
compileBitdata name univ def =
  do let tname = dataName name
     let ty    = mkT tname []
     let cname = structConName name

     let deriv = standardDeriving -- Hm, equality in the presence of wildcards?
     let w = toInteger (BDD.width univ)
     let repT = compileMonoType (tWord w)
     let con = TH.normalC cname [TH.bangT repT]
     dataD <- TH.newtypeD (pure []) tname [] Nothing con [deriv]

     cvtIs <- [d| instance RTS.Convert $ty $ty where
                    convert = id
                    {-# INLINE convert #-}

                  instance RTS.Bitdata $ty where
                    type instance BDWidth $ty = $(TH.litT (TH.numTyLit w))
                    fromBits = $(TH.conE cname)
                    {-# INLINE fromBits #-}
                    toBits $(TH.conP cname [[p| x |]]) = x
                    {-# INLINE toBits #-}

                  instance RTS.Convert $ty $repT where
                    convert = toBits
                    {-# INLINE convert #-}


                  instance RTS.Convert $repT $ty where
                    convert = fromBits
                    {-# INLINE convert #-}
                |]

     hasIs <- case def of
                BDStruct fs -> traverse (hasInstanceStruct ty cname) fs
                BDUnion cs  -> traverse (hasInstanceUnion ty) cs

     pure (dataD : cvtIs ++ concat hasIs)

  where

  hasInstanceStruct ty cname f =
    case bdFieldType f of
      BDWild     -> pure []
      BDTag {}   -> pure []
      BDData l t ->
        do ft <- compileMonoType t

           x  <- TH.newName "x"
           let p     = TH.conP cname [ TH.varP x ]
               lab   = TH.litT (TH.strTyLit (Text.unpack l))
               amt   = bdOffset f
               wt    = compileMonoType (tWord (toInteger (bdWidth f)))

           [d| instance R.HasField $lab $ty $(pure ft) where
                 getField $p = RTS.fromBits (
                                 RTS.convert
                                    ($(TH.varE x) `RTS.shiftr` RTS.UInt amt)
                                 :: $wt)
                 {-# INLINE getField #-}
             |]


  hasInstanceUnion ty (l,t) =
    do let lty = TH.litT (TH.strTyLit (Text.unpack l))
       ft <- compileMonoType t
       [d| instance R.HasField $lty $ty $(pure ft) where
              getField = RTS.fromBits . RTS.toBits
              {-# INLINE getField #-}

           instance RTS.Convert $(pure ft) $ty where
              convert = fromBits . toBits
              {-# INLINE convert #-}
          |]



