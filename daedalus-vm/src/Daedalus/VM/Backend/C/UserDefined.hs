{-# Language OverloadedStrings, BlockArguments #-}
module Daedalus.VM.Backend.C.UserDefined where

import qualified Data.Set as Set
import Data.List(partition)
import Data.Maybe(catMaybes)
import Data.Map(Map)
import qualified Data.Map as Map

import Daedalus.PP
import Daedalus.Rec
import Daedalus.Panic(panic)

import Daedalus.Core
import Daedalus.Core.Free
import Daedalus.VM.Backend.C.Lang
import Daedalus.VM.Backend.C.Names
import Daedalus.VM.Backend.C.Types
import Daedalus.VM.Backend.C.Bitdata


-- | Returns (declaration, methods)
cTypeGroup :: NSUser => Map TName TDecl -> Rec TDecl -> (Doc,Doc)
cTypeGroup allTypes rec =
    case rec of
      NonRec d ->
        case tDef d of
          TStruct {} -> ( cUnboxedProd d
                        , generateMethods GenPublic GenUnboxed d
                        )
          TUnion {}  -> ( cSumTags [d] $$ cUnboxedSum GenPublic d
                        , generateMethods GenPublic GenUnboxed d
                        )

          TBitdata univ def -> ( vcat' [ cBitdata allTypes d univ def
                                       , decShow GenPublic d
                                       , decShowJS GenPublic d
                                       ]
                               , vcat' [ defShow allTypes GenPublic d
                                       , defShowJS allTypes GenPublic d
                                       ]
                               )

      MutRec ds ->
        ( vcat' $
          -- 1. Declare names of types
          map (cTypeDecl GenPublic) (sums ++ prods) ++
          map (cTypeDecl GenPrivate) sums ++

          -- 2. Declare tags
          [ cSumTags sums ] ++

          -- 3. Declare boxed sums
          map cBoxedSum sums ++

          -- 4. Declare products
          map cUnboxedProd prods ++

          -- 5. Declare unboxed sums
          map (cUnboxedSum GenPrivate) sums

        , vcat' $
          -- 6. Generate methods
          map (generateMethods GenPrivate GenUnboxed) sums ++
          map (generateMethods GenPublic GenUnboxed) prods ++
          map (generateMethods GenPublic GenBoxed) sums
        )

        where
        (sums,prods) = orderRecGroup ds



{- Note: Product types shouldn't be directly recursive as that would
require infinite values, which we do not support.   There may be recursive
groups only containing products though: for example, if the recursion happens
through an array: a = [a]

A recursive sum type is represented with 3 types:
  A) an unboxed sum type describing the data
  B) a boxed pointer to A
  C) an enumeration type for the tags
All types in the group may depend on the B) part of the type.
Only the B) part of a type can depend on the A) part.


The types in a recursive group are declared in this order:
  1. Add declarations (without definitions) for all types in the group
    (without enums)
  2. Declare enums
  3. Add class declarations (without methods) for the B) parts of sum types
  4. Add class declarations (without methods) for the struct types,
     in dependency order (without the sums these should not be recursive)
  5. Add class declarations (without methods) for the A) parts of sum types.
-}
orderRecGroup :: [TDecl] -> ([TDecl],[TDecl]) -- ^ (sums,products)
orderRecGroup tds = (sums, forgetRecs (topoOrder deps prods))
  where
  (sums,prods) = partition isSum tds
  isSum t = case tDef t of
              TUnion  {} -> True
              TStruct {} -> False
              TBitdata {} -> panic "orderRecGroup" [ "bitdata" ]

  sumSet  = Set.fromList (map tName sums)
  deps td = (tName td, freeTCons td `Set.difference` sumSet)


cUserNS :: NSUser => GenVis -> [Doc] -> Doc
cUserNS vis = cNamespace nsUser
            . case vis of
                GenPublic -> id
                GenPrivate -> (:[]) . cNamespace nsPrivate

cTypeDecl :: NSUser => GenVis -> TDecl -> CDecl
cTypeDecl vis ty = cUserNS vis [ cTypeDecl' ty <.> semi ]

-- Note: this does not add the semi at the end, so we can reuse it in `Def`.
-- Note: This does not add the namespace
cTypeDecl' :: TDecl -> CDecl
cTypeDecl' ty =
  vcat [ cTemplateDecl ty, "class" <+> cTNameRoot (tName ty) ]

cTemplateDecl :: TDecl -> Doc
cTemplateDecl ty =
  case cTypeParams ty of
    [] -> empty
    ps -> cInst "template" ps

cTypeParams :: TDecl -> [Doc]
cTypeParams ty =
  map intP (tTParamKNumber ty) ++ map tyP (tTParamKValue ty)
  where
  intP x = "int"      <+> cTParam x
  tyP x  = "typename" <+> cTParam x




-- Example: Node<T>
cTypeNameUse :: NSUser => GenVis -> TDecl -> CType
cTypeNameUse vis tdecl =
  cTypeUse (cTNameUse vis (tName tdecl))
           (map cTParam (tTParamKNumber tdecl))
           (map cTParam (tTParamKValue tdecl))


--------------------------------------------------------------------------------
-- Signatures

-- | Signature for the @copy@ method
copyMethodSig :: Doc
copyMethodSig = cStmt ("void" <+> cCall "copy" [])

-- | Signature for the @free@ method
freeMethodSig :: Doc
freeMethodSig = cStmt ("void" <+> cCall "free" [])

-- | Constructor for a product
cProdCtr :: NSUser => TDecl -> CStmt
cProdCtr tdecl = cStmt ("void" <+> cCall structCon params)
  where params = [ cSemType t | (_,t) <- getFields tdecl, t /= TUnit ]

cProdSels :: NSUser => TDecl -> [ CStmt ]
cProdSels tdecl =
  [ cStmt (cSemType t <+> cCall nm []) | (f,t) <- getFields tdecl
                                       , pref  <- bor_own
                                       , let nm = selName pref f
                                       ]
  where
  bor_own = if tnameBD (tName tdecl) then [ GenOwn ] else [GenBorrow,GenOwn]


-- | Signatures for getters of a sum
cSumGetters :: NSUser => TDecl -> [CStmt]
cSumGetters tdecl =
  [ cStmt (cSemType t <+> cCall (selName pref l) [])
  | (l,t) <- getFields tdecl
  , pref <- [GenBorrow,GenOwn]
  ]

-- | Signatures for constructors of a sum
cSumCtrs :: NSUser => TDecl -> [CStmt]
cSumCtrs tdecl =
  [ cStmt ("void" <+> cCall (unionCon l)
                                    [ cSemType ty | ty <- [t], t /= TUnit ])
  | (l,t) <- getFields tdecl
  ]





--------------------------------------------------------------------------------
-- Unboxed Products

-- | Interface definition for struct types
cUnboxedProd :: NSUser => TDecl -> CDecl
cUnboxedProd ty = vcat (theClass : decFunctions GenPublic ty)
  where
  theClass =
    cNamespace nsUser
      [ cTypeDecl' ty <+> ": public" <+> (nsDDL .:: "HasRefs") <+> "{"
      , nest 2 $ vcat attribs
      , "public:"
      , nest 2 $ vcat methods
      , "};"
      ]

  TStruct fields = tDef ty
  attribs = [ cStmt (cSemType t <+> cField n)
            | ((_,t),n) <- zip fields [ 0 .. ], t /= TUnit
            ]
  methods =
       [ "/** @name Constructor */"
       , "///@{" ]
    ++ [ cProdCtr ty ]
    ++ [ "///@}"
       , ""
       , "/** @name Selectors */"
       , "///@{" ]
    ++ cProdSels ty
    ++ [ "///@}"
       , ""
       , "/** @name Memory Management */"
       , "///@{" ]
    ++ [ copyMethodSig, freeMethodSig ]
    ++ [ "///@}"]



--------------------------------------------------------------------------------
-- Tags

-- | Declare a bunch of tag types
cSumTags :: [TDecl] -> CDecl
cSumTags sums
  | null sums = empty
  | otherwise = cNamespace nsDDL [ cNamespace nsTag (map cSumTag sums) ]

  where
  cSumTag ty =
    fsep $ [ "enum class", cTNameRoot (tName ty), "{" ] ++
           punctuate comma (map (cLabel . fst) fields) ++
           [ "};" ]
    where
    TUnion fields = tDef ty

-- | The type of tags for the given type
cSumTagT :: TDecl -> CType
cSumTagT tdecl = nsDDL .:: nsTag .:: cTNameRoot (tName tdecl)

cSumTagV :: TName -> Label -> Doc
cSumTagV t l = nsDDL .:: nsTag .:: cTNameRoot t .:: cLabel l

-- | @getTag@ method signature
cSumGetTag :: TDecl -> Doc
cSumGetTag td = cStmt (cSumTagT td <+> cCall "getTag" [])
--------------------------------------------------------------------------------


-- | Class signature for an unboxed sum
cUnboxedSum :: NSUser => GenVis -> TDecl -> CDecl
cUnboxedSum vis tdecl = vcat (theClass : decFunctions vis tdecl)
  where
  theClass =
    cUserNS vis
      [ cTypeDecl' tdecl <+> ":" <+> (nsDDL .:: "HasRefs") <+> "{"
      , nest 2 $ vcat attribs
      , "public:"
      , nest 2 $ vcat methods
      , "};"
      ]


  fields = getFields tdecl

  attribs =
    [ cStmt $ cSumTagT tdecl <+> "tag" ] ++
    [ cStmt $ vcat
        [ "union Data {"
        , nest 2 $ vcat $ map cStmt [ cSemType t <+> cField n | (t,n) <- fs ]
        , nest 2 "Data() {}"
        , "} ddl_data"
        ]
    | let fs = [ (t,n) | ((_,t),n) <- zip fields [ 0 .. ], t /= TUnit ]
    , not (null fs)
    ]

  methods =
       [ "/** @name Constructors */"
       , "///@{" ]
   ++ cSumCtrs tdecl
   ++ [ "///@}"
       , ""
      , "/** @name Selectors */"
      , "///@{" ]
   ++ [ cSumGetTag tdecl ]
   ++ cSumGetters tdecl
   ++ [ "///@}"
       , ""
      , "/* @name Memory Management */"
      , "///@{" ]
   ++ [ copyMethodSig, freeMethodSig ]
   ++ [ "///@}"
       , ""
      , "/* @name Variant dispatch */"
      , "///@{" ]
   ++ cSumPats tdecl fields
   ++ [cSumCaseDecl]
   ++ [ "///@}" ]

cSumPats :: TDecl -> [(Label, Type)] -> [CDecl]
cSumPats td fs =
  [
    cUsingT (cPatSingleton l) (cInst "DDL::Pat" [cSumTagT td, cSumTagV (tName td) l])
  | (l,_) <- fs
  ]

cPatSingleton :: Label -> CIdent
cPatSingleton l = "Pat_" <> cLabel l

cUnboxedSumSwitch :: NSUser => GenVis -> TDecl -> [(Label, Type)] -> CDecl
cUnboxedSumSwitch vis tdecl fs =
  vcat
    [ cTemplateDecl tdecl
    , cTemplate ["typename Cases"] $
      cDefineFun "auto" (cTypeNameUse vis tdecl .:: "sum_switch")
                        ["Cases&& cases"] [
        vcat
          [ "switch" <+> parens (cCall "getTag" []) <+> "{"
          , nest 2 $ vcat
                [ "case" <+> cSumTagV (tName tdecl) l <.> colon
                  $$ nest 2 (cReturn (cCall "cases" [cCallCon (cPatSingleton l) [], cCall (selName GenBorrow l) []]))
                | (l,_) <- fs
                ]
          , "}"
          ]
      ]
    ]

cBoxedSumSwitch :: NSUser => GenVis -> TDecl -> CDecl
cBoxedSumSwitch vis tdecl =
  vcat
    [ cTemplateDecl tdecl
    , cTemplate ["typename Cases"] $
      cDefineFun "auto" (cTypeNameUse vis tdecl .:: "sum_switch")
                        ["Cases&& cases"] [
        cReturn (cCallMethod (cCallMethod "ptr" "getValue" [])
                                                    "sum_switch" ["cases"])
      ]
  ]

cSumCaseDecl :: CDecl
cSumCaseDecl =
  cTemplate ["typename Cases"] $
  cDeclareFun "auto" "sum_switch" ["Cases&& cases"];

-- | Class signature for a boxed sum
cBoxedSum :: NSUser => TDecl -> CDecl
cBoxedSum tdecl = vcat (theClass : decFunctions GenPublic tdecl)
  where
  theClass =
    cNamespace nsUser
      [ cTypeDecl' tdecl <+> ": public" <+> (nsDDL .:: "IsBoxed") <+> "{"
      , nest 2 $ vcat attrs
      , "public:"
      , nest 2 $ vcat methods
      , "};"
      ]


  attrs =
    [ cStmt
        $ cInst (nsDDL .:: "Boxed") [ cTypeNameUse GenPrivate tdecl ] <+> "ptr"
    ]

  methods =
       [ "/** @name Constructors */"
       , "///@{" ]
    ++ cSumCtrs tdecl
    ++ [ "///@}"
       , ""
       , "/** @name Selectors */"
       , "///@{" ]
    ++ [ cSumGetTag tdecl ]
    ++ cSumGetters tdecl
    ++ [ "///@}"
       , ""
       , "/** @name Memory Management */"
       , "///@{" ]
    ++ [ copyMethodSig, freeMethodSig ]
    ++ [ "///@}"
       , ""
       , "/** @name Variant dispatch */"
       , "///@{" ]
    ++ cSumPats tdecl (getFields tdecl)
    ++ [cSumCaseDecl]
    ++ [ "///@}"]

--------------------------------------------------------------------------------
-- Method definitions
--------------------------------------------------------------------------------

data GenBoxed = GenBoxed  | GenUnboxed


decFunctions :: NSUser => GenVis -> TDecl -> [CDecl]
decFunctions vis ty =
  [ decCompare vis ty
  , decCmpOp "==" vis ty
  , decCmpOp "!=" vis ty
  , decCmpOp "<" vis ty
  , decCmpOp ">" vis ty
  , decCmpOp "<=" vis ty
  , decCmpOp ">=" vis ty
  ] ++
  [ decShow vis ty
  , decShowJS vis ty
  ]


generateMethods :: NSUser => GenVis -> GenBoxed -> TDecl -> Doc
generateMethods vis boxed ty =
  vcat' $
    [ "// --- Methods for" <+> pp (tName ty) <+> "------------------"
    , defCopyFree vis boxed "copy" ty
    , defCopyFree vis boxed "free" ty
    ] ++
    defCons      vis boxed ty ++
    defGetTag    vis boxed ty ++
    defSelectors vis boxed ty ++
    [ defCompare vis ty
    , defCmpOp "==" vis ty
    , defCmpOp "!=" vis ty
    , defCmpOp "<" vis ty
    , defCmpOp ">" vis ty
    , defCmpOp "<=" vis ty
    , defCmpOp ">=" vis ty
    ] ++
    [defShow Map.empty vis ty, defShowJS Map.empty vis ty] ++
    defSwitch vis boxed ty

defSwitch :: NSUser => GenVis -> GenBoxed -> TDecl -> [CDecl]
defSwitch vis boxed ty =
  case tDef ty of
    TUnion fs ->
      case boxed of
        GenBoxed -> [cBoxedSumSwitch vis ty]
        GenUnboxed -> [cUnboxedSumSwitch vis ty fs]
    _ -> []

defMethod ::
  NSUser => GenVis -> TDecl -> CType -> Doc -> [Doc] -> [CStmt] -> CDecl
defMethod vis tdecl retT fun params def =
  vcat [ cTemplateDecl tdecl
       , "inline"
       , retT <+> cCall name params <+> "{"
       , nest 2 (vcat def)
       , "}"
       ]
  where
  name = cTypeNameUse vis tdecl <.> "::" <.> fun

--------------------------------------------------------------------------------
-- Output

decShow :: NSUser => GenVis -> TDecl -> CDecl
decShow vis tdecl =
  cUserNS vis
  [ cTemplateDecl tdecl
  , "inline"
  , cDeclareFun "std::ostream&" "operator <<" [ "std::ostream&", ty ]
  ]
  where
  ty = cTypeNameUse vis tdecl

-- XXX: for boxed things we could delegate, but that would require
-- friendship to access `ptr`
defShow :: NSUser => Map TName TDecl -> GenVis -> TDecl -> CDecl
defShow allTypes vis tdecl =
  cUserNS vis
  [ cTemplateDecl tdecl
  , "inline"
  , cDefineFun "std::ostream&" "operator <<" [ "std::ostream& os", ty <+> "x" ]
    case tDef tdecl of

      TStruct fs ->
        showStruct False fs

      TUnion fs ->
        [ cStmt ("os <<" <+> cString "{| ")
        , cSwitch (cCallMethod "x" "getTag" [])
             [ cCase (cSumTagV (tName tdecl) l) (showUnionCase False f)
             | f@(l,_) <- fs ]
        , cStmt ("return os <<" <+> cString " |}")
        ]

      TBitdata univ def ->
        case def of
          BDStruct fs -> showStruct True
                          [ (l,t) | f <- fs, BDData l t <- [bdFieldType f ] ]
          BDUnion fs ->
            [ cStmt ("os <<" <+> cString "{| ")
            , bdCase True allTypes univ "x"
              [ (t, showUnionCase True f)
              | f@(_,t) <- fs ]
            , cStmt ("return os <<" <+> cString " |}")
            ]
    ]
  where
  ty = cTypeNameUse vis tdecl

  showStruct bd fs =
    let how = if bd then GenOwn else GenBorrow
        end = case fs of
                [] -> "{}" :: String
                _  -> " }"
    in
    [ cStmt ("os <<" <+> fld <+> "<<" <+> thing)
    | ((l,_),sepa) <- fs `zip` ("{" : repeat ",")
    , let thing = cCallMethod "x" (selName how l) []
          fld   = cString (show (sepa <+> pp l <+> "= "))
    ] ++
    [ cStmt ("return os <<" <+> cString end) ]

  showUnionCase bd (l,_) =
    let lab = cString (show (pp l <+> "= "))
        how = if bd then GenOwn else GenBorrow
        val = cCallMethod "x" (selName how l) []
    in vcat $ cStmt ("os <<" <+> lab <+> "<<" <+> val)
            : if bd then [] else [cBreak]
              -- break added by bdCase for bitdata




decShowJS :: NSUser => GenVis -> TDecl -> CDecl
decShowJS vis tdecl = 
  cNamespace nsDDL
   [ cTemplateDecl tdecl, "inline",
      cDeclareFun "std::ostream&" "toJS" [ "std::ostream&", ty ]
   ]
  where
  ty = cTypeNameUse vis tdecl

-- | Show in ppshow friendly format
defShowJS :: NSUser => Map TName TDecl -> GenVis -> TDecl -> CDecl
defShowJS allTypes vis tdecl =
  cNamespace nsDDL
    [ cTemplateDecl tdecl
    , "inline"
    ,  cDefineFun "std::ostream&" "toJS" [ "std::ostream& os", ty <+> "x" ]
       case tDef tdecl of

         -- assumes at least one field
         TStruct fs -> defShowStruct False fs

         TUnion fs ->
           [ vcat [ "switch" <+> parens (cCallMethod "x" "getTag" []) <+> "{"
                  , nest 2 $ vcat
                       [ "case" <+> cSumTagV (tName tdecl) l <.> colon
                          $$ nest 2 (defShowUnionCase False f)
                       | f@(l,_) <- fs
                       ]
                  , "}"
                  ]
           , cStmt "return os"
           ]

         TBitdata univ def ->
           case def of
             BDStruct fs ->
              defShowStruct True [ (l,t)
                                 | f <- fs, BDData l t <- [bdFieldType f] ]
             BDUnion fs ->
               [ bdCase True allTypes univ "x"
                   [ (t,defShowUnionCase True f) | f@(_,t) <- fs ]
               , cStmt "return os"
               ]

      ]
  where
  ty = cTypeNameUse vis tdecl

  defShowStruct bd fs =
    let how = if bd then GenOwn else GenBorrow
        end = case fs of
                [] -> "{}" :: String
                _  -> " }"
    in
    [ cStmt (cCall (nsDDL .:: "toJS") ["os <<" <+> fld, thing])
    | ((l,_),sepa) <- fs `zip` ("{" : repeat ",")
    , let thing = cCallMethod "x" (selName how l) []
          fld   = cString (show (sepa <+> cString (show (pp l)) <.> ": "))
    ] ++
    [ cStmt ("return os <<" <+> cString end) ]

  defShowUnionCase bd (l,_t) =
    let lab = '$' : show (pp l)
        how = if bd then GenOwn else GenBorrow
        val = cCallMethod "x" (selName how l) []
        addBreak xs = if bd then xs else xs $$ cBreak
    in addBreak $
       vcat [ cStmt ("os <<" <+> cString ("{ " ++ show lab ++ ": "))
            , cStmt (cCall (nsDDL .:: "toJS") [ "os", val ])
            , cStmt "os << \"}\""
              ]




--------------------------------------------------------------------------------
-- Comparisons

decCompare :: NSUser => GenVis -> TDecl -> CDecl
decCompare vis tdecl =
  cNamespace nsDDL
    [ cTemplateDecl tdecl
    , cDeclareFun "static inline int" "compare" [ ty, ty ]
    ]
  where
  ty = cTypeNameUse vis tdecl

defCompare :: NSUser => GenVis -> TDecl -> CDecl
defCompare vis tdecl =
  cNamespace nsDDL
    [ cTemplateDecl tdecl
    , cDefineFun "static inline int" "compare" [ ty <+> "x", ty <+> "y" ] body
    ]
  where
  ty = cTypeNameUse vis tdecl
  body =
    case tDef tdecl of
      TStruct fs ->
        concat
        [ [cDeclareVar "int" "result"]
        , [ s
          | (f,_) <- fs
          , let m = selName GenBorrow f
          , s <-
              [ cAssign "result" (cCall (nsDDL .:: "compare")
                                    [ cCallMethod "x" m []
                                    , cCallMethod "y" m []
                                    ])
              , cIf' "result != 0" [cReturn "result"]
              ]
          ]
        , [cReturn "0"]
        ]
      TUnion fs ->
        [ cSwitch "x.getTag()"
            [ cCaseBlock (cSumTagV (tName tdecl) f)
              [ cDeclareInitVar "auto" "tag" "y.getTag()"
              , cIf' (cSumTagV (tName tdecl) f <+> "< tag") [ cReturn "-1" ]
              , cIf' (cSumTagV (tName tdecl) f <+> "> tag") [ cReturn "1" ]
              , let get a = cCallMethod a (selName GenBorrow f) []
                in cReturn (cCall (nsDDL .:: "compare") [ get "x", get "y" ])
              ]

            | (f,_) <- fs
            ]
        , cUnreachable
        ]

      TBitdata {} -> [ "// XXX: define `compare` on bitdata" ]

decCmpOp :: NSUser => Doc -> GenVis -> TDecl -> CDecl
decCmpOp op vis tdecl =
  cUserNS vis
  [ cTemplateDecl tdecl
  , cDeclareFun "static inline bool" ("operator" <+> op)
                      [ ty <+> "x", ty <+> "y" ]
  ]
  where ty = cTypeNameUse vis tdecl

defCmpOp :: NSUser => Doc -> GenVis -> TDecl -> CStmt
defCmpOp op vis tdecl =
  cUserNS vis
  [ cTemplateDecl tdecl
  , cDefineFun "static inline bool" ("operator" <+> op)
                      [ ty <+> "x", ty <+> "y" ]
    [ cReturn (cCall (nsDDL .:: "compare") ["x","y"] <+> op <+> "0") ]
  ]
  where ty = cTypeNameUse vis tdecl



--------------------------------------------------------------------------------
-- Construcotrs

defCons :: NSUser => GenVis -> GenBoxed -> TDecl -> [CDecl]
defCons vis boxed tdecl =
  case tDef tdecl of
    TStruct {} -> [ defStructCon vis boxed tdecl ]
    TUnion  {} -> defUnionCons vis boxed tdecl
    TBitdata {} -> panic "defCons" ["bitdata"]


defStructCon :: NSUser => GenVis -> GenBoxed -> TDecl -> CDecl
defStructCon vis boxed tdecl = defMethod vis tdecl "void" structCon params def
  where
  params = [ t <+> x | (t,x,_) <- fs ]
  def =
    case boxed of
      GenBoxed   -> [ cStmt $ cCallMethod "ptr" "allocate" []
                    , cStmt $ cCallMethod "ptr.getValue()" structCon
                                                      [ x | (_,x,_) <- fs ]
                    ]
      GenUnboxed -> [ cAssign f x | (_,x,f) <- fs ]

  fs = [ (cSemType t, cLabel l, cField n)
       | ((l,t),n) <- getFields tdecl `zip` [ 0 .. ]
       , t /= TUnit
       ]

defUnionCons :: NSUser => GenVis -> GenBoxed -> TDecl -> [CDecl]
defUnionCons vis boxed tdecl = zipWith defCon (getFields tdecl) [ 0 .. ]
  where
  defCon (l,t) n =
    let name = unionCon l
        fs   = [ (cSemType t, cLabel l) | t /= TUnit ]
    in defMethod vis tdecl "void" name [ ty <+> x | (ty,x) <- fs ]
       case boxed of
         GenBoxed -> [ cStmt (cCall "ptr.allocate" [])
                     , cStmt (cCall ("ptr.getValue()." <.> name) (map snd fs))
                     ]
         GenUnboxed ->
            cStmt ("tag =" <+> cSumTagV (tName tdecl) l)
          : [ cAssign (cSelect "ddl_data" (cField n)) (cLabel l) | t /= TUnit ]


--------------------------------------------------------------------------------
-- Selectors


defSelectors :: NSUser => GenVis -> GenBoxed -> TDecl -> [CDecl]
defSelectors vis boxed tdecl =
  concatMap (defSelectorsOwn vis boxed tdecl) [GenBorrow,GenOwn]


defGetTag :: NSUser => GenVis -> GenBoxed -> TDecl -> [CDecl]
defGetTag vis boxed tdecl =
  case tDef tdecl of
    TStruct {} -> []
    TUnion {} ->
      [ defMethod vis tdecl (cSumTagT tdecl) "getTag" []
          [ case boxed of
              GenBoxed   -> cReturn (cCallMethod "ptr.getValue()" "getTag" [])
              GenUnboxed -> cReturn "tag"
          ]
      ]
    TBitdata {} -> ["// XXX: get tag for bitdata"]

-- XXX: Maybe some selectors should return a reference?
defSelectorsOwn :: NSUser => GenVis -> GenBoxed -> TDecl -> GenOwn -> [CDecl]
defSelectorsOwn vis boxed tdecl borrow =
  case tDef tdecl of
    TBitdata {} -> panic "defSelectorsOwn" [ "bitdata" ]
    _ -> zipWith sel (getFields tdecl) [ 0 .. ]
  where

  uni  = case tDef tdecl of
           TStruct _ -> False
           TUnion  _ -> True
           TBitdata {} -> panic "defSelectorsOwn" [ "bitdata" ]


  sel (l,t) n =
    let name       = selName borrow l
        uniNote    = if uni
                       then "\nOnly valid when getTag() is " <+> cSumTagV (tName tdecl) l
                       else ""
        doc        = vcat [ "/** Get the value of field `" <.> pp l <.> "`."
                          , uniNote
                          , "*/"
                          ]
    in doc $$ (
       defMethod vis tdecl (cSemType t) name []
       case boxed of
         GenBoxed ->
           [ cReturn (cCallMethod "ptr.getValue()" name []) ]
         GenUnboxed ->
           [ s | GenOwn <- [borrow], Just s <- [ maybeCopyFree "copy" t f ] ] ++
           [ cReturn
             case t of
               TUnit -> nsDDL .:: "Unit()"
               _     -> f
           ]
           where f = if uni then "ddl_data." <.> cField n else cField n
        )

--------------------------------------------------------------------------------




--------------------------------------------------------------------------------
-- Copy & Free

-- | Define a copy/free method for the given type
defCopyFree :: NSUser => GenVis -> GenBoxed -> Doc -> TDecl -> CDecl
defCopyFree vis boxed fun tdecl =
  case tDef tdecl of
    TBitdata {} -> empty
    _ -> defMethod vis tdecl "void" fun []
        case boxed of
          GenBoxed -> [ cStmt (cCallMethod "ptr" fun []) ]
          GenUnboxed ->
            case tDef tdecl of
              TStruct _ -> map snd (stmts True)
              TUnion _ ->
                case stmts False of
                  [] -> []
                  xs -> [ cSwitchDefault (cCall "getTag" [])
                            [ cCase (cSumTagV (tName tdecl) l) (s $$ cBreak)
                            | (l,s) <- xs
                            ]
                            cBreak
                        ]
              TBitdata {} -> []

  where
  stmts struct =
    let dat = if struct then empty else "ddl_data."
    in
    catMaybes
       [ do s <- maybeCopyFree fun t (dat <.> cField n)
            pure (l,s)

       | ((l,t),n) <- getFields tdecl `zip` [ 0.. ]
       ]


-- | Emit some code to copy/free a field, unless it is Unit
-- as those are deleted.
maybeCopyFree :: Doc -> Type -> CExpr -> Maybe CStmt
maybeCopyFree fun ty e =
  case ty of
    TUnit -> Nothing  -- these are deleted
    _     -> Just (cStmt (cCallMethod e fun []))

