{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
{-# Language BlockArguments #-}
module HTML where
import Prelude hiding (div)

import Data.List(intersperse)
import Data.Ratio(numerator,denominator)

import Types

specToHTML :: Spec -> HTML
specToHTML s = fieldsToHTML (sName s) (sFields s)

fieldsToHTML :: String -> [Field] -> HTML
fieldsToHTML nm fs =
  tag "html" [] $ htmls
    [ tag "head" [] $ tag' "link" [ ("rel","stylesheet"), ("href","style.css") ]
    , tag "body" [] $ htmls $ div "heading" (htmlText nm) : map fieldToHTML2 fs
    ]


fieldToHTML2 :: Field -> HTML
fieldToHTML2 fld = div "field" $ htmls $
  [ div "bounds" bounds
  , div "key" (fieldPatHTML (fName fld))
  , htmlText "::"
  , typeToHTML (fType fld)
  ] ++
  [ htmlText ", required" | fRequired fld ] ++
  [ htmlText ", indirect" | fIndirect fld ] ++
  [ htmlText ", value used" | fValNeeded fld ] ++
  [ htmls [ htmlText ", default = ", exprToHTML x ]
                                          | Just x <- [fDefaultValue fld] ]
  where
  bounds = htmls [ from, to ]
  from = htmlText (show (fSince fld))
  to   = case fDeprecated fld of
           Just x -> htmlText ("--" ++ show x)
           Nothing -> htmlText ""

fieldPatHTML :: FieldPat -> HTML
fieldPatHTML fp =
  case fp of
    PArrayAny -> htmlText "[*]"
    PArrayIx n -> htmlText ("[" ++ show n ++ "]")
    PFieldName x  -> htmlText x

primTyToHTML :: PrimType -> HTML
primTyToHTML ty =
  case ty of
    TInteger     -> htmlText "Integer"
    TNumber      -> htmlText "Number"
    TName        -> htmlText "Name"
    TBool        -> htmlText "Bool"
    TStringText  -> htmlText "Text"
    TStringByte  -> htmlText "Bytes"
    TStringAscii -> htmlText "ASCII"
    TString      -> htmlText "String"
    TRectangle   -> htmlText "Rectangle"
    TDate        -> htmlText "Date"
    TNull        -> htmlText "Null"

structTyToHTML :: StructType -> HTML
structTyToHTML ty =
  case ty of
    TArray       -> htmlText "Array"
    TStream      -> htmlText "PDFStream"
    TDictionary  -> htmlText "Dictionary"
    TNameTree    -> htmlText "NameTree"
    TNumberTree  -> htmlText "NumberTree"

typeToHTML :: Type -> HTML
typeToHTML ty =
  case ty of
    TOr t1 t2    -> htmls [ typeToHTML t1, " or ", typeToHTML t2 ]
    TPrim t mbc ->
      case mbc of
        Nothing -> primTyToHTML t
        Just c -> htmls [ "(", primTyToHTML t, " | ", constraintToHTML c, ")" ]
    TStruct t mb ->
      case mb of
        Nothing -> structTyToHTML t
        Just l ->
          htmls [ structTyToHTML t, htmlText " ",
                    tag "a" [ ("href", l ++ ".html") ] (htmlText l) ]

constraintToHTML :: Constraint -> HTML
constraintToHTML c =
  case c of
    Orc c1 c2 ->
      htmls [ constraintToHTML c1, htmlText " or ", constraintToHTML c2 ]
    Equals x -> htmls [ htmlText "= ", exprToHTML x ]
    Interval x y -> htmls $ [ htmlText "["
                            , div "literal" (htmlText (show x))
                            , htmlText ".."
                            ] ++
                            [ div "literal" (htmlText (show v))
                                      | Just v <- [y] ] ++
                            [ htmlText "]" ]

    IsGreaterThan e -> htmls [ htmlText "> ", exprToHTML e ]
    IsLessThan e    -> htmls [ htmlText "< ", exprToHTML e ]

fieldIxToHTML :: FieldIx -> HTML
fieldIxToHTML i =
  case i of
    ArrayIx n -> htmlText ("@" ++ show n)
    FieldIx f -> htmlText ("@" ++ show f)


exprToHTML :: Expr -> HTML
exprToHTML e =
  case e of
    ValueOf i -> fieldIxToHTML i
    ELit x    -> lit x
    ELitI x   -> lit x
    ELitR x   -> div "literal"
              $ htmls [ htmlText (show (numerator x))
                      , htmlText "/"
                      , htmlText (show (denominator x))
                      ]
    EBool x    -> lit x
    ELitStr x  -> lit x
    ELitName x -> lit ('/' : x)
    EArr es    -> htmls ( htmlText "["
                        : intersperse (htmlText ", ") (map exprToHTML es)
                       ++ [ htmlText "]" ]
                        )
  where
  lit x = div "literal" $ htmlText $ show x

--------------------------------------------------------------------------------
type HTML = String

htmlText :: String -> HTML
htmlText = concatMap esc1
  where
  esc1 c = case c of
             '&' -> "&amp;"
             '<' -> "&lt;"
             '>' -> "&gt;"
             '"' -> "&quot;"
             _   -> [c]

htmls :: [HTML] -> HTML
htmls = unlines

tag :: String -> [(String,String)] -> HTML -> HTML
tag t as b = tag' t as ++ b ++ concat [ "</", t, ">" ]

tag' :: String -> [(String,String)] -> HTML
tag' t as = concat [ "<", t, " ", unwords (map attr as), ">" ]
  where attr (k,v) = k ++ "=" ++ show (htmlText v)

div :: String -> HTML -> HTML
div x = tag "div" [("class",x)]


