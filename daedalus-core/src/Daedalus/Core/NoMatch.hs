{-# Language OverloadedStrings #-}
module Daedalus.Core.NoMatch (noMatch) where

import Daedalus.GUID(HasGUID)

import Daedalus.Core
import Daedalus.Core.Type(sizeType)
import Control.Monad (join)


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
     def <- traverse (flip desugarByteSet (Var x)) (fDef fu)
     pure fu { fParams = x : fParams fu
             , fDef = def
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
         b <- freshNameSys TBool
         let msg = byteArrayL "Leftover input"
         pure $ Do i GetStream
              $ Let b (isEmptyStream (Var i))
              $ coreIf b
                    (Pure unit)
                    (Fail ErrorFromSystem TUnit (Just msg))

    MatchBytes e ->
      do i <- freshNameSys TStream
         let bytes = TArray (TUInt (TSize 8))
         x <- freshNameSys bytes
         b <- freshNameSys TBool

         let msg = eConcat (arrayL bytes [ byteArrayL "Expected ", e ])
             advance = SetStream (eDrop (arrayLen (Var x)) (Var i))
             (t,ok) = case s of
                       SemNo  -> (TUnit,advance)
                       SemYes -> (bytes, Do_ advance (Pure (Var x)))
         pure $ Do i GetStream
              $ Let x e
              $ Let b (isPrefix (Var x) (Var i))
              $ coreIf b
                    ok
                    (Fail ErrorFromSystem t (Just msg))

    MatchByte b ->
      do i <- freshNameSys TStream
         let byte = TUInt (TSize 8)
         x <- freshNameSys byte
         p <- freshNameSys TBool
         p' <- freshNameSys TBool
         
         pv <- desugarByteSet b (Var x)
         let msgNoByte = byteArrayL "Unexpected end of input"
             msgBadByte = byteArrayL "Byte does not match specification"
             advance = SetStream (eDrop (intL 1 sizeType) (Var i))
             (t,ok) = case s of
                        SemNo  -> (TUnit, advance)
                        SemYes -> (byte, Do_ advance (Pure (Var x)))
         pure $ Do i GetStream
              $ Let p (isEmptyStream (Var i))
              $ coreIf p
                    (Fail ErrorFromSystem t (Just msgNoByte))
              $ Let x (eHead (Var i))
              $ Let p' pv
              $ coreIf p'
                    ok
                    (Fail ErrorFromSystem t (Just msgBadByte))


eOr, eAnd :: HasGUID m => Expr -> Expr -> m Expr
eOr x y = do
  p <- freshNameSys TBool
  pure (PureLet p x (coreIf p (boolL True) y))
  
eAnd x y = do
  p <- freshNameSys TBool
  pure (PureLet p x (coreIf p y (boolL False)))

desugarByteSet :: HasGUID m => ByteSet -> Expr -> m Expr
desugarByteSet bs b =
  case bs of
    SetAny        -> pure (boolL True)
    SetSingle x   -> pure (x `eq` b)
    SetRange x y  -> (x `leq` b) `eAnd` (b `leq` y)
    SetComplement x -> eNot <$> desugarByteSet x b
    SetUnion x y -> join (eOr <$> desugarByteSet x b <*> desugarByteSet y b)
    SetIntersection x y -> join (eAnd <$> desugarByteSet x b <*> desugarByteSet y b)
    SetLet x e y -> PureLet x e <$> desugarByteSet y b -- assumes no capture
    SetCall f es -> pure (callF f (b:es))
    SetCase cs -> ECase <$> traverse (flip desugarByteSet b) cs


