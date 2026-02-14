module ExportCPP where

import Data.Map(Map)
import Data.Map qualified as Map
import Data.Void(vacuous)
import Text.PrettyPrint

import Daedalus.PP
import Daedalus.Core qualified as Core
import Daedalus.VM.Backend.C.Lang
import Daedalus.VM.Backend.C.Names
import Daedalus.VM.Backend.C.Types
import Daedalus.VM.Backend.C.UserDefined
import Name
import Quote
import AST
import Type

type Ctx = (
  NSUser,
  ?ftAliases :: Map QName ForeignTypeDecl,
  ?ddlTPMap :: Map Name Core.TParam
  )

genModules :: Ctx => [Module DDLTCon QName] -> (Doc,Doc)
genModules mos = (header,impl)
  where
  header =
    vcat $
      [ "#pragma once" ] ++
      optBanner "User Specified Custom Code" (foreigns False) ++
      optBanner "Exporter Declarations" exportDecls

  impl =
    vcat $
      optBanner "User Specified Custom Code" (foreigns True) ++
      optBanner "Exporter Definitions" exportDefs
  
  optBanner x ys =
    case ys of
      [] -> []
      _  -> banner x ++ ys

  banner x = [
    " ",
    "// ---------------------------------------------",
    "// " <+> x,
    "// ---------------------------------------------",
    " "
    ]

  foreigns withDef =
    [ renderQuote (vacuous (foreignCode f))
    | mo <- mos
    , f <- moduleForeign mo
    , foreignDef f == withDef
    ]

  inNS mo = cNamespace (pp (moduleName mo))
  exportDecls = [ inNS mo [ cStmt (genDeclPart d) | d <- moduleDecls mo ]
                | mo <- mos ]
  exportDefs = [ inNS mo (map genDeclDef (moduleDecls mo)) | mo <- mos ]


genForeignType :: Ctx => Type QName -> CType
genForeignType ty =
  case ty of
    TVar x -> cForeignTP x
    Type tc args sizes
      | null sizes ->
        case Map.lookup (locThing tc) ?ftAliases of
          Just tdef -> renderQuote (ppP <$> ftDef tdef)
            where
            xs = map locThing (ftParams tdef)
            mp = Map.fromList (zip xs (map genForeignType args))
            ppP x =
              case Map.lookup (locThing x) mp of
                Just ty1 -> ty1
                Nothing -> error "[BUG] genForeignType: missing param" 
          Nothing -> error $ unlines $ ("[BUG] genForeignType: missing type alias: " ++ show (pp tc))
                                     : [ show (pp k <+> ":" <+> pp v) | (k,v) <- Map.toList ?ftAliases ]
      | otherwise -> error "[BUG] genForeignType: sizes"

type DDLTPs = Map Name Core.TParam

genDDLType :: Ctx => Type DDLTCon -> CType
genDDLType = cSemType . typeToCoreType ?ddlTPMap

cForeignTP :: Loc Name -> Doc
cForeignTP = pp . locThing

cFunTP :: Int -> Doc
cFunTP n = "Fn" <.> pp n

-- XXX: module qualifiers, namespaces
cDeclName :: QName -> Doc
cDeclName x = pp (qName x)

getDDLTPs :: Decl DDLTCon QName -> [(Name, Core.TParam)]
getDDLTPs decl = map locThing (declDDLTParams decl) `zip` map Core.TP [ 0 .. ]
  


genDeclPart :: Ctx => Decl DDLTCon QName -> CDecl
genDeclPart decl =
    template_decl (
      genForeignType resT <+> cDeclName (locThing (declName decl)) <.>
        parens (commaSep params)
    )
  where
  template_decl =
    case ctps of
      [] -> id
      _  -> cTemplate [ "typename" <+> tp | tp <- ctps ]

  argT :-> resT = declType decl

  params = funParams ++ [valParam]
  funParams = [ t <+> pp (fst f) | (f,t) <- declFunParams decl `zip` funTPs ]
  valParam =
    let ?ddlTPMap = Map.fromList ddlTPs
    in genDDLType argT <+> pp (locThing (declArg decl))

  ctps = map (cTParam . snd) ddlTPs ++
         map cForeignTP (declForeignTParams decl) ++
         funTPs
         
  funTPs = zipWith (\n _ -> cFunTP n) [ 0 .. ] (declFunParams decl)
  ddlTPs = getDDLTPs decl
  

genDeclDef :: Ctx => Decl DDLTCon QName -> Doc
genDeclDef decl =
  let ?ddlTPMap = Map.fromList (getDDLTPs decl) in
  case declDef decl of
    DeclDef fc -> doDecl (genForeignCode fc)
    DeclCase x alts -> doDecl (genCase x a alts)
    DeclLoop loop -> doDecl (genLoop loop a)
    DeclExtern -> mempty
  where
  a :-> _     = declType decl 
  doDecl def  = vcat [ genDeclPart decl <+> "{", nest 2 def, "}\n" ]

genLoop :: Ctx => Loop DDLTCon QName -> Type DDLTCon -> CStmt
genLoop loop ty =
  vcat (
    renderQuote (cForeignTP <$> loopInit loop) :
    loopBody ++
    [ cReturn (renderQuote (vacuous (loopReturn loop))) ]
  )
  where
  loopBody =
    case ty of
      Type tc args nargs ->
        [ cDeclareConVar cit cix[pp (locThing x)],
          cWhile ("!" <.> cCallMethod cix "done" [])
            (cBlock (declareEls ++ [genForeignCode' body] ++ updIt)),
          cStmt (cCallMethod cix "free" [])
        ]
        where
        cit = "typename" <+> genDDLType (Type tc { locThing = TIterator } [ ty ] [])
        cix = pp (locThing x) <.> "_iter"
        (els,x,body) = loopFor loop
        doDecl a el m =
          cDeclareInitVar (genDDLType a) (pp (locThing el)) (cCallMethod cix m [])

        updIt = [ cAssign cix (cCallMethod cix "next" []) ]

        declareEls =
          case (locThing tc,els,args,nargs) of
            (TArray,[el],[a],[]) ->
              [ doDecl a el "value" ]

            (TMap,[kel,vel],[k,v],[]) ->
              [ doDecl k kel "key", doDecl v vel "value" ]
            
            _ -> error "[BUG] genLoop: unexpected loop type"

      _ -> error "[BUG] genLoop: unexpected TVar"
      

genCase :: Ctx => Loc Name -> Type DDLTCon -> [(Pat DDLTCon,ForeignCode DDLTCon QName)] -> CStmt
genCase x ty alts =
  let xe = genDDLExpr (DDLExpr x [])
      altMap = Map.fromList [ (nameToText (locThing l), (f,a)) | (PCon l f,a) <- alts ]
      getAlt a =
        case Map.lookup a altMap of
          Just e -> e
          Nothing -> error "[BUG] genCase: unknown case"
      getSimpleAlt a =
        case getAlt a of
          (Nothing,v) -> v
          _ -> error "[BUG] genCase: unexpected field in pattern"
      getFieldAlt a =
        case getAlt a of
          (Just (f,_),v) -> (locThing f,v)
          _ -> error "[BUG] genCase: missing field pattern"
  in
  case ty of
    Type tc targs nargs ->
      case (locThing tc, targs, nargs) of
        (TBool, [], []) ->

          cIf (cCallMethod xe "getValue" [])
            [genForeignCode trueCase]
            [genForeignCode falseCase]
          where
          trueCase = getSimpleAlt "true"
          falseCase = getSimpleAlt "false"
          
        (TMaybe,[a],[]) ->
          cIf (cCallMethod xe "isNothing" [])
            [genForeignCode nothingCase]
            [ cDeclareInitVar (genDDLType a) (pp field) (cCallMethod xe "getValue" []),
              genForeignCode justCase
            ]
          where
          nothingCase = getSimpleAlt "nothing"
          (field, justCase) = getFieldAlt "just"

        (TUser u,_,_) ->
          cSwitch (cCallMethod xe "getTag" []) (map doCase alts)
          where
          freeTop sel =
            [ cIf (cCallMethod xe "refCount" [] <+> "==" <+> "1")
                (sel GenBorrow ++ [ cStmt (cCallMethod xe "del" []) ])
                (sel GenOwn ++ [ cStmt (cCallMethod xe "free" []) ])
            ]
          doCase (PCon l f, rhs) =
            let lab = nameToText (locThing l)
                tag = cSumTagV u lab
                (declareLocal, initLocal) =
                  case f of
                    Nothing -> ([],const [])
                    Just (fi,Just t) ->
                      let v = pp fi in
                      ( [ cDeclareVar (genDDLType t) v ],
                        \how -> [ cAssign v (cCallMethod xe (selName how lab) []) ]
                      )
                    Just _ -> error "[BUG] genCase: no type on pattern"
            in
            cCaseBlock tag (declareLocal ++ freeTop initLocal ++ [genForeignCode rhs])
          
        _ -> error "[BUG] genCase: unexpected type"
    TVar {} -> error "[BUG] genCase: unexpected type variable"

genForeignCode :: Ctx => ForeignCode DDLTCon QName -> CStmt
genForeignCode = cReturn . genForeignCode'

genForeignCode' :: Ctx => ForeignCode DDLTCon QName -> CExpr
genForeignCode' co =
  case co of
    Splice e -> renderQuote (genForeignCodeSplice <$> e)
    Direct e -> genForeignExpr e

genForeignCodeSplice :: Ctx => ForeignCodeSplice DDLTCon QName -> Doc
genForeignCodeSplice spl =
  case spl of
    SpliceTParam x -> cForeignTP x
    SpliceCode e -> genForeignExpr e

genForeignExpr :: Ctx => ExportExpr DDLTCon QName -> CExpr
genForeignExpr e =
  case exportWith e of
    Just ex -> genExporter ex (Just (genDDLExpr (exportExpr e)))
    Nothing -> "[BUG] genForeignExpr: unresolved default"

-- | Generate code for calling an exporter with the given argument
genExporter :: Ctx => Exporter DDLTCon QName -> Maybe CExpr -> CExpr
genExporter ex arg =
  case ex of
    ExportTop f dts fts args ty ->
      case arg of
        Just e  -> doCall e
        Nothing ->
          case ty of
            Just (a :-> _) -> "[&]" <.> parens (genDDLType a <+> eta) <+> "mutable {" <+> cReturn (doCall eta) <+> "}"
              where
              eta = "exp_arg"
            Nothing -> error "[BUG] genExporter: no type"
      where
      tyArgs = map genDDLType dts ++ map genForeignType fts
      base = pp (locThing f)
      fun = if null tyArgs then base else cInst base tyArgs
      funArgs = [ genExporter fa Nothing | fa <- args ]
      doCall x = cCall fun (funArgs ++ [x])
      
    ExportLocal f _ty ->
      case arg of
        Nothing -> fun
        Just e  -> cCall fun [e]
      where
      fun = pp (locThing f)

genDDLExpr :: DDLExpr -> CExpr
genDDLExpr (DDLExpr x sel) =
  case sel of
    [] -> v
    [StructSelector :. l] ->
      cCallMethod v (selName GenBorrow (nameToText (locThing l))) []
    _ -> error "[XXX] genDDLExpr: more selector support"
  where
  v = pp (locThing x)

