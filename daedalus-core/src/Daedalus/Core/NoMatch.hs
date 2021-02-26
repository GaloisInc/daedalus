{-# Language OverloadedStrings #-}
module Daedalus.Core.NoMatch where

import Daedalus.GUID(HasGUID,invalidGUID)

import Daedalus.Core


noMatch :: HasGUID m => Grammar -> m Grammar
noMatch g =
  do g1 <- childrenG noMatch g
     case g1 of
       Match s m -> desugarMatch s m
       _         -> pure g1

desugarMatch :: HasGUID m => Sem -> Match -> m Grammar
desugarMatch s mat =
  case mat of
    MatchBytes e ->
      do i <- freshName Name { nameId = invalidGUID
                             , nameText = Nothing
                             , nameType = TStream
                             }
         let bytes = TArray (TUInt (TSize 8))
             (t,v) = case s of
                       SemNo  -> (TUnit,unit)
                       SemYes -> (bytes, e)
             msg = eConcat $ arrayL bytes [ byteArrayL "Expected ", e ]
         pure $ Do i GetStream
              $ GCase
              $ Case (isPrefix e (Var i))
                  [ (PBool True, Pure v)
                  , (PBool False, Fail ErrorFromSystem t (Just msg))
                  ]



