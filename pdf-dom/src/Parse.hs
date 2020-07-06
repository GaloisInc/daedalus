{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language BlockArguments #-}
module Parse where

import Prelude hiding (div)

import Data.Char(toLower, isAlphaNum, isAlpha, isDigit)
import Data.Ratio((%))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as CSV
import Data.Foldable(toList)
import Text.ParserCombinators.ReadP
import Control.Monad(guard)

import Debug.Trace


import Types
import Utils

parseFile :: String -> FilePath -> IO Spec
parseFile nm file =
  do fs <- parseBytes =<< LBS.readFile file
     pure (orderSpecFields Spec { sName = nm
                                , sFields = fs
                                , sShapes = []
                                })

parseBytes :: LBS.ByteString -> IO [Field]
parseBytes bs =
  case CSV.decodeByNameWithP parser opts bs of
    Left err -> fail err
    Right (_,vs) -> pure (toList vs)
  where
  opts = CSV.defaultDecodeOptions { CSV.decDelimiter = toEnum (fromEnum ';') }

parser :: CSV.NamedRecord -> CSV.Parser Field
parser m =
  do fName <- parseFieldPat <$> txt "Key"
     fTypeBase <- parseType <$> txt "Type"
     fType1 <- parseConstraint fTypeBase <$> txt "PossibleValues"
     fType  <- parseLink fName fType1 <$> txt "Link"
     fSince <- parseVersion <$> txt "SinceVersion"
     fDepr <- txt "DeprecatedIn"
     let fDeprecated = if null fDepr then Nothing else Just (parseVersion fDepr)
     fRequired <- bool "Required"
     fIndirect <- bool "IndirectReference"
     fDef <- txt "DefaultValue"
     let fDefaultValue =
            if null fDef
               then Nothing
               else case runP pExpr fDef of
                     Just e -> Just e
                     Nothing -> trace ("WARNING: ignoring malformed default value: " ++ show (fDef, fTypeBase)) Nothing
     let fValNeeded = False -- (computed later, see orderSpecFields)
     pure Field { .. }
  where
  txt x  = do s <- m CSV..: x
              pure (s :: String)
  bool x = do s <- txt x
              pure (map toLower s == "true")


parseVersion :: String -> Float
parseVersion x = case reads x of
                   [(n,"")] -> n
                   _ -> error ("Inavlid version: " ++ show x)

parseType :: String -> Type
parseType s
  | (x,_:y) <- break (== ';') s = TOr (parseType x) (parseType y)
  | otherwise =
  case s of
    "integer"       -> tPrim TInteger
    "number"        -> tPrim TNumber
    "name"          -> tPrim TName
    "boolean"       -> tPrim TBool
    "rectangle"     -> tPrim TRectangle
    "date"          -> tPrim TDate
    "null"          -> tPrim TNull
    "string-text"   -> tPrim TStringText
    "string-byte"   -> tPrim TStringByte
    "string-ascii"  -> tPrim TStringAscii
    "string"        -> tPrim TString
    "array"         -> tStruct TArray
    "dictionary"    -> tStruct TDictionary
    "stream"        -> tStruct TStream
    "name-tree"     -> tStruct TNameTree
    "number-tree"   -> tStruct TNumberTree
    _ -> error ("Unknown type: " ++ show s)
  where
  tPrim t = TPrim t Nothing
  tStruct t = TStruct t Nothing


runP :: ReadP a -> String -> Maybe a
runP p s =
  case [ c | (c,"") <- readP_to_S p s ] of
    [c] -> Just c
    _ -> Nothing

parseFieldPat :: String -> FieldPat
parseFieldPat c =
  case runP pFieldPat c of
    Just t1 -> t1
    Nothing -> error $ "malfored field name " ++ c

pFieldPat :: ReadP FieldPat
pFieldPat =
  (string "[*]" >> pure PArrayAny) <++
  (between (char '[') (char ']') (PArrayIx <$> readS_to_P reads)) <++
  (PFieldName <$> munch1 (const True))
  

parseLink :: FieldPat -> Type -> String -> Type
parseLink f ty s
  | null s = ty
  | otherwise =
    case runP (pLink f ty) s of
      Just t1 -> t1
      Nothing -> error $ "unknown link: " ++ show (f,s,ty)

pLink :: FieldPat -> Type -> ReadP Type
pLink f ty =
  case ty of
    TOr t1 t2 ->
      do x1 <- pLink f t1
         _  <- char ';'
         x2 <- pLink f t2
         pure (TOr x1 x2)
    _ ->
       do l <- between (char '[') (char ']')
                  (munch1 isLinkChar `sepBy` char ',')
          pure $ if null l
                  then ty
                  else case ty of
                         TPrim t _ -> trace msg ty
                           where msg = "WARNING: ignoring link " ++ show l ++
                                       " on primitive type " ++ show t ++
                                       " in field " ++ show f
                         TOr {} -> error "Unexpected TOr"
                         TStruct s ~Nothing ->
                          foldr1 TOr [ TStruct s (Just n) | n <- l ]

  where
  isLinkChar x = isAlphaNum x || x == '_'

parseConstraint :: Type -> String -> Type
parseConstraint ty s
  | null s = ty
  | otherwise =
  case runP (pTopCtr ty) s of
    Just c -> c
    Nothing -> error $ "Unknown constraint: " ++ show (s,ty)


pTopCtr :: Type -> ReadP Type
pTopCtr ty =
  case ty of
    TOr t1 t2 ->
      do xs <- pTopCtr t1
         _ <- char ';'
         ys <- pTopCtr t2
         pure (TOr xs ys)
    _ -> do mb <- pCtr1
            pure $ case mb of
                     Nothing -> ty
                     Just c -> case ty of
                                 TPrim x Nothing -> TPrim x (Just c)
                                 _ -> error "Constraint on structured type"

pCtr1 :: ReadP (Maybe Constraint)
pCtr1 = choice [ do xs@(_ : _ : _) <- pLitCtr `sepBy1` char ','
                    pure (Just (foldr1 Orc xs))
               , pCtr
               ]


pLit :: ReadP Expr
pLit =
  ratLit  <++
  numLit  <++
  intLit  <++
  boolLit <++
  nameLit <++
  strLit
  where
  strLit = do xs <- munch1 identChar
              guard (any isAlpha xs)
              pure (ELitStr xs)
  identChar c = isAlphaNum c || c == '-' || c == '.' || c == '(' || c == ')'

  nfb p = do xs <- look
             case xs of
               [] -> pure ()
               x : _ -> guard (not (p x))

  boolLit = EBool <$> choice [ string "FALSE" >> nfb isAlphaNum >> pure False
                             , string "TRUE" >> nfb isAlphaNum >> pure True
                             ]

  intLit = do x <- readS_to_P reads
              next <- look
              case next of
                 '.' : d : _ | isDigit d -> fail ""
                 c : _ | isAlpha c -> fail ""
                 _ -> pure ()
              pure (ELitI x)

  numLit = do x <- readS_to_P reads
              nfb isAlphaNum
              pure (ELit x)

  nameLit = do _ <- char '/'
               xs <- munch1 identChar
               pure (ELitName xs)

  ratLit =
    do x <- readS_to_P reads
       _ <- char '/'
       y <- readS_to_P reads
       pure (ELitR (x % y))



pLitCtr :: ReadP Constraint
pLitCtr = Equals <$> pLit

pCtr :: ReadP (Maybe Constraint)
pCtr =
  do x <- choice
        [ foldr1 mkOr <$> between (char '[') (char (']'))
                                 (pCtr `sepBy1` char ',')
        , string "[]" >> pure Nothing
        , Just <$> pLitCtr
        , Just <$> ltCtr
        , Just <$> gtCtr
        , Just <$> interval
        , ignoreTransform
        ]
     optional (char ' ' >> ignoreTransform)
     pure x

  where
  mkOr x y =
    case (x,y) of
      (Nothing,_) -> y
      (_,Nothing) -> x
      (Just a, Just b) -> Just (Orc a b)

  num = readS_to_P reads :: ReadP Double

  ltCtr = do _ <- string "value<"
             IsLessThan <$> pExpr
  gtCtr = do _ <- string "value>"
             IsGreaterThan <$> pExpr

  ignoreTransform = string "value*" >> pExpr >> pure Nothing

  interval = between (char '<') (char '>') intervalRange
        +++ between (char '(') (char ')') intervalRange
        +++ between (char '<') (char ')') intervalRange   -- ???

  intervalRange =
    do x <- num
       _ <- string ".." +++ string ","
       y <- option Nothing (Just <$> num)
       pure (Interval x y)

pExpr :: ReadP Expr
pExpr = choice
  [ string "@" >> ValueOf . ArrayIx <$> readS_to_P reads
  , string "@" >> ValueOf . FieldIx <$>
                                ((:) <$> satisfy isAlpha <*> munch isAlphaNum)
  , pLit
  , EArr <$> between (char '[' >> skipSpaces) (skipSpaces >> char ']')
                     (pExpr `sepBy` sep)
  ]
  where
  sep = munch1 \x -> x == ' ' || x == ','





