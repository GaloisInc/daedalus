{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language TypeOperators #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language StandaloneDeriving #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language OverloadedStrings #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
module Test where
 
import RTS.Parser
import qualified Prelude as HS
import qualified GHC.TypeLits as HS
import qualified GHC.Records as HS
import qualified Control.Monad as HS
import qualified RTS as RTS
import qualified RTS.Vector as Vector
import qualified RTS.Map as Map
 
 
pMain :: Parser (RTS.UInt 8)
 
pMain =
  do RTS.pSetInput (RTS.arrayStream (Vector.fromList [RTS.uint8 65]))
     (__ :: RTS.UInt 8) <-
       RTS.uint8
         HS.<$> RTS.pMatch1 "4:3--4:5" (RTS.bcSingle (RTS.uint8 65))
     HS.pure __
 
_Main :: Parser ()
 
_Main =
  do RTS.pSetInput (RTS.arrayStream (Vector.fromList [RTS.uint8 65]))
     HS.const ()
       HS.<$> RTS.pMatch1 "4:3--4:5" (RTS.bcSingle (RTS.uint8 65))