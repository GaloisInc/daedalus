{-# Language OverloadedStrings #-}
module DDL where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad(foldM)
import System.FilePath(addExtension,(</>))
import Numeric(showEFloat)
import Data.List(foldl')

import Debug.Trace(trace)

import Daedalus.Rec
import Daedalus.PP

import Types
import Utils
import Parse(parseFile)

parseTypes :: FilePath -> [String] -> IO [Mod]
parseTypes dir roots = (specsToMods . order) <$> parseMany Map.empty roots
  where
  order xs = let ys   = map (fmap snd) (topoOrder fst (Map.elems xs))
                 uses = tyUses (forgetRecs ys)
                 ann x = x { sShapes = Map.findWithDefault [] (sName x) uses }
             in map (fmap ann) ys

  parseMany = foldM parseOne

  parseOne done nm
    | nm `Map.member` done = pure done
    | otherwise =
      do spec <- parseFile nm (addExtension (dir </> nm) ".csv")
         let deps = tyDeps spec
             node = (nm,deps)
         parseMany (Map.insert nm (node,spec) done) (Set.toList deps)

specsToMods :: [Rec Spec] -> [Mod]
specsToMods rs = map toMod modNames
  where
  defMap   = foldl' defines Map.empty modNames
  modNames = (zip [ 0 .. ] (reverse rs))

  defines mp (n,r) =
    case r of
      NonRec s  -> def1 n mp s
      MutRec ss -> foldl' (def1 n) mp ss

  def1 n mp s = Map.insert (sName s) n mp

  imps s = Set.fromList [ defMap Map.! x | x <- Set.toList (tyDeps s) ]

  toMod (n,r) =
    let ss = recToList r
    in Mod { mName = n
           , mSpecs = ss
           , mImports = Set.toList $ Set.unions $ map imps ss
           }



--------------------------------------------------------------------------------

makeCabal :: FilePath -> Int -> IO ()
makeCabal dir n = writeFile (addExtension (dir </> name) "cabal") content
  where
  name = "pdf-dom-validator"
  content =
    unlines $
      [ "cabal-version:       3.0"
      , "name:                " ++ name
      , "version:             0.1.0.0"
      , "license:             ISC"
      , "license-file:        LICENSE"
      , "author:              Iavor Diatchki"
      , "maintainer:          iavor.diatchki@gmail.com"
      , "copyright:           2019, Galois Inc"
      , "build-type:          Simple"
      , ""
      , "library"
      , "  default-language:     Haskell2010"

      , "  signatures:"
      , "    PdfValidator"
      , ""
      , "  exposed-modules:"
      ] ++
      [ "    " ++ modName i ++ suff
        | i <- take n [ 0 .. ]
        , let suff = if i == n - 1 then "" else ","
      ] ++
      [ "  build-depends:"
      , "    base,"
      , "    rts-hs"
      ]

--------------------------------------------------------------------------------

modName :: Int -> String
modName x = "PdfDom" ++ pad 3 (show x)
  where
  pad n xs = replicate (n - length xs) '0' ++ xs

saveMod :: FilePath -> Mod -> IO ()
saveMod outDir m =
  do let n  = modName (mName m)
         is = [ "import PdfValidate"
              , "import PdfDecl"
              , "import PdfValue"
              ] ++ [ "import" <+> text (modName x) | x <- mImports m ] ++
              [ "" ]
         file = outDir </> addExtension n "ddl"
     writeFile file $ show $ vcat $ is ++ map checkSpec (mSpecs m)


checkSpec :: Spec -> DDL
checkSpec spec = vcat
  [ "def" <+> validatorName name <+> "(v : Value) = {"
  , nest 2 checkRef
  , "} <|" <+> validatorDoName name <+> "v"
  , ""
  , "def" <+> validatorDoName name <+> "(v : Value) = {"
  , nest 2 doDef
  , "}"
  ]
  where
  name      = sName spec
  tag       = typeName name

  -- NOTE: a bunch of this could be reused if we added higher (2nd) order
  -- functions
  checkRef  = vcat
    [ "@r = v is ref;"
    , "commit;"
    , "@done = IsValidated r.obj r.gen" <+> tag <.> semi
    , "Default {} {"
    , nest 2 $ vcat
        [ "done is false;"
        , "commit;"
        , "StartValidating r.obj r.gen" <+> tag <.> semi
        , "@val = ResolveValRef r;"
        , validatorDoName name <+> "val;"
        ]
    , "}"
    ]
  doDef =
    case sShapes spec of
      [] -> checkDict spec
      [TDictionary] -> checkDict spec
      ty -> "{ {- XXX: SHAPE" <+> text (show ty) <+> "-} }"


checkDict :: Spec -> DDL
checkDict spec =
  vcat ("@d = v is dict;" :  map (checkDictField "d") (sFields spec))

checkDictField :: DDL -> Field -> DDL
checkDictField d f
  | fValNeeded f = error "Value needed not yet implemented"
  | otherwise =
    case fName f of
      PFieldName s
        | fRequired f -> ("{" <+> check s) $$ "};"
        | otherwise   -> "Default {} {" $$ nest 2 (check s) $$ "};"
      _ -> error "Unexpected array pattern in a dictionary spec"

  where
  check s = vcat
              [ "@f = Lookup" <+> fieldName s <+> d <.> ";"
              , "commit;"
              , checkFieldType s "f" (fType f) <.> ";"
              ]

checkFieldType :: String -> DDL -> Type -> DDL
checkFieldType fname fvar ty =
  case ty of
    TOr t1 t2 -> vcat [ checkFieldType fname fvar t1 <+> "<|"
                      , checkFieldType fname fvar t2
                      ]

    TPrim pt mbc ->
      case mbc of
        Nothing -> "@" <+> checkPrimType fvar pt
        Just c  ->
          "{" <+> ("@pv =" <+> checkPrimType fvar pt <.> ";"
                    $$ checkConstraint pt "pv" c)
          $$ "}"
    TStruct st mb ->
      case mb of
        Nothing ->
          case st of
            TDictionary  -> "@{" <+> fvar <+> "is dict; }"
            TArray       -> "@{" <+> fvar <+> "is array; }"
            TStream      -> "{{- XXX: Streams -}}"
            TNumberTree  -> "{{- XXX: Number tree -}}"
            TNameTree    -> "{{- XXX: Name tree -}}"
        Just s  -> validatorName s <+> fvar


checkPrimType :: DDL -> PrimType -> DDL
checkPrimType var ty = "{ @rv = ResolveVal" <+> var <.> ";" <+> check <+> "}"
  where
  check =
    case ty of
      TInteger        -> "CheckInteger rv;"
      TNumber         -> "rv is number;"
      TName           -> "rv is name;"
      TBool           -> "rv is bool;"
      TStringText     -> "CheckText rv;"
      TStringByte     -> "rv is string;"
      TString         -> "rv is string;"
      TStringAscii    -> "CheckASCII rv;"
      TRectangle      -> "CheckRectangle rv;"
      TDate           -> "CheckDate rv;"
      TNull           -> "rv is null;"


checkConstraint :: PrimType -> DDL -> Constraint -> DDL
checkConstraint pt x c =
  case c of
    Equals e           -> eq x (exprToDDL pt e)
    Interval l Nothing -> leq (exprToDDL pt (ELit l)) x
    Interval l (Just u) -> "{" <+> leq (exprToDDL pt (ELit l)) x <.> ";" <+> 
                                   leq x (exprToDDL pt (ELit u)) <+> "}"
    IsLessThan e    -> lt x (exprToDDL pt e)
    IsGreaterThan e -> lt (exprToDDL pt e) x
    Orc c1 c2       -> (checkConstraint pt x c1 <+> "<|") $$
                        checkConstraint pt x c2

  where
  eq a b = case pt of
            TInteger -> a <+> "==" <+> b
            TName    -> a <+> "==" <+> b
            TNumber  -> "EqNumber" <+> a <+> b
            TStringAscii -> a <+> "==" <+> b
            TString      -> a <+> "==" <+> b
            _        -> error ("XXX: Equal on " ++ show pt)

  leq a b = case pt of
              TInteger -> a <+> "<=" <+> b
              _        -> trace ("XXX: <= on type " ++ show pt)
                                ("{ {-" <+> a <+> "<=" <+> b <+> "-} }")

  lt a b  = case pt of
              TInteger -> a <+> "<" <+> b
              _        -> trace ("XXX: < on type " ++ show pt)
                              ("{ {-" <+> a <+> "<" <+> b <+> "-} }")


exprToDDL :: PrimType -> Expr -> DDL
exprToDDL pt expr =
  case expr of
    ELitI n     -> case pt of
                     TName    -> strToDDL (show n)
                     TString  -> strToDDL (show n)
                     TInteger -> intToDDL n
                     _        -> error ("Int lit at type " ++ show pt)
    ELit d      -> case pt of
                     TName    -> strToDDL (show d)
                     TString  -> strToDDL (show d)
                     TInteger -> let i = round d
                                 in if fromInteger i == d
                                      then intToDDL i
                                      else error "Double is not integer"
                     TNumber  -> dblToDDL d
                     _ -> error ("Double lit at type " ++ show pt)

    ELitR r     -> error "Unnexpected Rational"
    EBool b     -> if b then "true" else "false"
    ELitStr s   -> strToDDL s
    ELitName s  -> strToDDL s
    EArr _      -> error "XXX: Array"
    ValueOf _   -> error "XXX: ValueOf"


--------------------------------------------------------------------------------
type DDL = Doc

validatorName :: String -> DDL
validatorName name = "Check" <.> text name

validatorDoName :: String -> DDL
validatorDoName name = "DoCheck" <.> text name

fieldName :: String -> DDL
fieldName = text . show

typeName :: String -> DDL
typeName = text . show

dblToDDL :: Double -> DDL
dblToDDL x = "{ num =" <+> nu <.> ", exp =" <+> ex <+> "}"
  where
  (front,_:back) = break (== 'e') $ showEFloat Nothing x ""
  (xs,_:ys)      = break (== '.') front
  nu             = text (xs ++ ys)
  ex             = text (show (read back - length ys))

intToDDL :: Integer -> DDL
intToDDL = text . show

strToDDL :: String -> DDL
strToDDL = text . show


