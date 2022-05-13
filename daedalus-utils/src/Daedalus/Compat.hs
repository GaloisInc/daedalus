{-# Language CPP, TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Daedalus.Compat where

import Data.ByteString(ByteString,pack,unpack)
import Language.Haskell.TH.Syntax as TH

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

