-- | Compatability shims for Template Haskell
{-# Language CPP, TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Daedalus.TH
  ( TH.Q
  , TH.Lift
  , TH.runQ
  , TH.addDependentFile
  , TH.ppr_list

  , TH.Name
  , TH.mkName
  , TH.newName

  , TH.Type
  , TH.TypeQ
  , TH.Kind
  , TH.varT
  , TH.appT
  , TH.conT
  , TH.litT
  , TH.forallT
  , TH.cxt

  , TH.numTyLit
  , TH.strTyLit

  , TH.ExpQ
  , TH.varE
  , TH.lamE
  , TH.letE
  , TH.appE
  , TH.appsE
  , TH.listE
  , TH.conE
  , TH.caseE
  , TH.litE

  , TH.integerL
  , TH.stringL

  , TH.PatQ
  , TH.litP
  , TH.conP
  , TH.recP
  , TH.varP
  , TH.sigP
  , TH.bangP

  , DataParam
  , ForallParam
  , plainTV
  , kindedTV
  , tvName

  , TH.DecsQ
  , TH.DecQ
  , TH.Dec
  , TH.newtypeD
  , TH.dataD
  , TH.funD
  , TH.sigD

  , BangType
  , bangT
  , TH.normalC

  , TH.match
  , TH.normalB
  , TH.clause

  , TH.DerivClause
  , TH.derivClause

  , TH.QuasiQuoter(..)
  , TH.location
  , TH.loc_start
  , TH.loc_filename

  ) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Quote as TH
import Data.ByteString(ByteString,pack,unpack)

type BangType = (TH.Bang, TH.Type)

bangT :: TH.Q TH.Type -> TH.Q BangType
bangT t = TH.bangType (TH.bang TH.noSourceUnpackedness TH.noSourceStrictness) t


#if !MIN_VERSION_bytestring(0,11,2)

-- | @since 0.11.2.0
instance TH.Lift ByteString where
  lift bs = [| pack ws |]
    where ws = unpack bs

#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

#endif


#if MIN_VERSION_template_haskell(2,17,0)
type DataParam    = TH.TyVarBndr ()
type ForallParam  = TH.TyVarBndr TH.Specificity

tvName :: TH.TyVarBndr a -> TH.Name
tvName v =
  case v of
    TH.PlainTV x _ -> x
    TH.KindedTV x _ _ -> x

class MkTyVarBndr a where
  plainTV  :: TH.Name -> TH.TyVarBndr a
  kindedTV :: TH.Name -> TH.Kind -> TH.TyVarBndr a

instance MkTyVarBndr () where
  plainTV = TH.plainTV
  kindedTV = TH.kindedTV

instance MkTyVarBndr TH.Specificity where
  plainTV x    = TH.PlainTV x TH.inferredSpec
  kindedTV x k = TH.KindedTV x TH.inferredSpec k

#else
type DataParam    = TH.TyVarBndr
type ForallParam  = TH.TyVarBndr

plainTV :: TH.Name -> TH.TyVarBndr
plainTV = TH.plainTV

kindedTV :: TH.Name -> TH.Kind -> TH.TyVarBndr
kindedTV = TH.kindedTV

tvName :: TH.TyVarBndr -> TH.Name
tvName v =
  case v of
    TH.PlainTV x -> x
    TH.KindedTV x _ -> x
#endif
