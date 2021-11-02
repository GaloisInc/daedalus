{-# Language GADTs, DataKinds, ExistentialQuantification, RecordWildCards #-}
module Daedalus.ParserGen.AST where

import Numeric (showHex)
import qualified Data.Map as Map
import Data.Text(Text)

import Daedalus.Type.AST hiding (ppBinder)
import Daedalus.SourceRange


name2Text :: Name -> Text
name2Text n =
  let x = nameScopedIdent n
  in case x of
    Unknown ident -> ident
    Local   ident -> ident
    ModScope _ ident -> ident

showName :: Name -> String
showName n =
  showName1 (nameScopedIdent n)
  where
    showName1 ((ModScope _ x)) = show x
    showName1 ((Unknown x)) = show x
    showName1 ((Local x)) = show x


type Contx = Name

data Annot = Annot
  { annotStates :: [Int]
  , annotContx :: Maybe Contx
  }
  deriving (Show)

emptyAnnot :: Annot
emptyAnnot = Annot { annotStates = [], annotContx = Nothing}

mkAnnot :: [Int] -> Contx -> Annot
mkAnnot lst ctx =
  Annot {annotStates = lst, annotContx = Just ctx}

getState :: Annot -> Int -> Int
getState ann i = (annotStates ann) !! i

statesToAnnot :: [Int] -> Annot
statesToAnnot lst = Annot { annotStates = lst, annotContx = Nothing}

getStatesLength :: Annot -> Int
getStatesLength ann = length $ annotStates ann

nameToContx :: Name -> Contx
nameToContx n = n

type NCExpr = TC (SourceRange, Annot) Class
type NVExpr = TC (SourceRange, Annot) Value

showNCExpr :: NCExpr -> String
showNCExpr c =
  case texprValue c of
    TCSetAny -> "*"
    TCSetSingle e ->
      case texprValue e of
        TCLiteral (LByte i _) _ ->
          let x = toInteger i in
          if (48 <= x && x <= 57) || (65 <= x && x <= 90) || (97 <= x && x <= 122)
          then (toEnum (fromIntegral i) :: Char) : ""
          else "x" ++ showHex (fromIntegral i :: Integer) ""
        _ -> "MATCH"
    _ -> "MATCH"


type GblAlloc = Map.Map Name (TCModule (SourceRange, Annot))

data CorV =
    ClassExpr (TC (SourceRange, Annot) Class)
  | ValueExpr (TC (SourceRange, Annot) Value)
  deriving (Show)

type GblFuns = Map.Map Name ([Name], CorV)

type GblGrammar = Map.Map Name (TCDecl (SourceRange, Annot))


showSourceRange :: SourceRange -> String
showSourceRange sr =
  "(" ++ show (sourceLine $ sourceFrom sr) ++
  "," ++ show (sourceColumn $ sourceFrom sr) ++
  ")-(" ++ show (sourceLine $ sourceTo sr) ++
  "," ++ show (sourceColumn $ sourceTo sr) ++
  ")"
