{-# Language OverloadedStrings #-}
module Daedalus.Core.NoMatch where

import Daedalus.GUID(HasGUID)

import Daedalus.Core


noMatch :: HasGUID m => Module -> m Module
noMatch mo =
  do fs <- mapM noMatchBFun (mBFuns mo)
     gs <- mapM noMatchGFun (mGFuns mo)
     pure mo { mGFuns = gs, mBFuns = [], mFFuns = mFFuns mo ++ fs }

noMatchGFun :: HasGUID m => Fun Grammar -> m (Fun Grammar)
noMatchGFun fu =
  case fDef fu of
    Def b    -> (\b1 -> fu { fDef = Def b1 }) <$> noMatchG b
    External -> pure fu


noMatchBFun :: HasGUID m => Fun ByteSet -> m (Fun Expr)
noMatchBFun fu =
  do x <- freshNameSys (TUInt (TSize 8))
     pure fu { fParams = x : fParams fu
             , fDef = case fDef fu of
                        Def b    -> Def (desugarByteSet b (Var x))
                        External -> External
             }


noMatchG :: HasGUID m => Grammar -> m Grammar
noMatchG g =
  do g1 <- childrenG noMatchG g
     case g1 of
       Match s m -> desugarMatch s m
       _         -> pure g1

desugarMatch :: HasGUID m => Sem -> Match -> m Grammar
desugarMatch s mat =

  case mat of

    MatchEnd ->
      do i <- freshNameSys TStream
         let msg = byteArrayL "Leftover input"
         pure $ Do i GetStream
              $ gIf (isEmptyStream (Var i))
                    (Pure unit)
                    (Fail ErrorFromSystem TUnit (Just msg))

    MatchBytes e ->
      do i <- freshNameSys TStream
         let bytes = TArray (TUInt (TSize 8))
         x <- freshNameSys bytes

         let msg = eConcat (arrayL bytes [ byteArrayL "Expected ", e ])
             advance = SetStream (eDrop (arrayLen (Var x)) (Var i))
             (t,ok) = case s of
                       SemNo  -> (TUnit,advance)
                       SemYes -> (bytes, Do_ advance (Pure (Var x)))
         pure $ Do i GetStream
              $ Let x e
              $ gIf (isPrefix (Var x) (Var i))
                    ok
                    (Fail ErrorFromSystem t (Just msg))

    MatchByte b ->
      do i <- freshNameSys TStream
         let byte = TUInt (TSize 8)
         x <- freshNameSys byte
         let msgNoByte = byteArrayL "Unexpected end of input"
             msgBadByte = byteArrayL "Byte does not match specification"
             advance = SetStream (eDrop (intL 1 TInteger) (Var i))
             (t,ok) = case s of
                        SemNo  -> (TUnit, advance)
                        SemYes -> (byte, Do_ advance (Pure (Var x)))
         pure $ Do i GetStream
              $ gIf (isEmptyStream (Var i))
                    (Fail ErrorFromSystem t (Just msgNoByte))
              $ Let x (eHead (Var i))
              $ gIf (desugarByteSet b (Var x))
                    ok
                    (Fail ErrorFromSystem t (Just msgBadByte))


desugarByteSet :: ByteSet -> Expr -> Expr
desugarByteSet bs b = go bs
  where
    go bs' = 
      case bs' of
        SetAny          -> boolL True
        SetSingle x     -> x `eq` b
        SetRange x y    -> (x `leq` b) `eAnd` (b `leq` y)
        SetComplement x -> eNot (go x)
        SetUnion x y    -> go x `eOr` go y
        SetIntersection x y -> go x `eAnd` go y
        SetLet x e y    -> PureLet x e (go y) -- assumes no capture
        SetCall f es    -> callF f (b:es)
        SetCase (Case e as def) -> ECase (Case e (go <$> as) (go <$> def))

