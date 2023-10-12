{-# Language GeneralizedNewtypeDeriving, ConstraintKinds, KindSignatures #-}
{-# Language RankNTypes #-}
{-# Language DataKinds #-}
{-# Language UndecidableInstances #-}
module PdfMonad.Release (Parser, runParser, DbgMode, pdfMain) where

import Data.Foldable(toList)
import Data.Kind(Constraint)

import qualified RTS.Parser as RTS
import qualified RTS.ParseError as RTS
import qualified RTS.Annot as RTS

import PdfMonad.Transformer as T


type DbgMode = () :: Constraint

newtype Parser a = P (PdfT (RTS.ParserG RTS.Annotation) a)
  deriving (Functor, Applicative, Monad, BasicParser, PdfParser)

runParser ::
  DbgMode =>
  ObjIndex -> Maybe EncContext -> Parser a -> Input -> IO (PdfResult a)
runParser objMap ec (P m) i =
  pure $! case res of
            NoResults err -> ParseErr err
            Results ans ->
              case toList ans of
                [(a,_)] -> ParseOk a
                xs  -> ParseAmbig (map fst xs)
  where
  res = RTS.runParser (runPdfT i objMap ec m) RTS.SingleError i

pdfMain :: (DbgMode => IO ()) -> IO ()
pdfMain io = io

