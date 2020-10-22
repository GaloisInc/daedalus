module Daedalus.ExportRuleRanges(jsModules,JS) where

import Data.List(intercalate)
import qualified Data.Text as Text

import Daedalus.SourceRange
import Daedalus.Rec

import Daedalus.AST

jsModules :: [Module] -> JS
jsModules = jsObj . concatMap jsModule

jsModule :: Module -> [(String,JS)]
jsModule m = [ jsRule q r | rs <- moduleRules m, r <- recToList rs ]
  where q = Text.unpack (moduleName m)

jsRule :: String -> Rule -> (String,JS)
jsRule q r = (jsName q (ruleName r), jsRange (ruleRange r))

jsName :: String -> Name -> JS
jsName q nm =
  case nameScopedIdent nm of
    Unknown i    -> jsIdent q i
    Local i      -> jsIdent q i  -- shouldn't happen
    ModScope m i -> jsIdent (Text.unpack m) i

jsIdent :: String -> Ident -> JS
jsIdent q i = q ++ "." ++ Text.unpack i

jsPos :: SourcePos -> JS
jsPos = jsInt . sourceIndex

jsRange :: SourceRange -> JS
jsRange r = jsArr [ jsPos (sourceFrom r), jsPos (sourceTo r) ]


--------------------------------------------------------------------------------
type JS = String

jsInt :: Int -> JS
jsInt = show

jsArr :: [JS] -> JS
jsArr xs = "[" ++ intercalate "," xs ++ "]"

jsObj :: [(String,JS)] -> JS
jsObj xs =
  case xs of
    [] -> "{}"
    [f] -> field "{" f ++ "}"
    f : fs -> unlines ( field "{" f : map (field ",") fs ++ [ "}" ])
  where
  field p (x,s) = p ++ " " ++ show x ++ ": " ++ s


