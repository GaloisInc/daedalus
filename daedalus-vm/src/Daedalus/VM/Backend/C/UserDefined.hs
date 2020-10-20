{-# Language OverloadedStrings, BlockArguments #-}
module Daedalus.VM.Backend.C.UserDefined where

import qualified Data.Set as Set
import Data.List(partition)
import Data.Maybe(catMaybes)

import Daedalus.PP
import Daedalus.Rec

import Daedalus.Core
import Daedalus.Core.Free
import Daedalus.VM.TypeRep
import Daedalus.VM.Backend.C.Lang
import Daedalus.VM.Backend.C.Names
import Daedalus.VM.Backend.C.Types


cTypeGroup :: Rec TDecl -> Doc
cTypeGroup rec =
    "\n// --------------------------------------------------------- //\n"
    $$
    case rec of
      NonRec d ->
        case tDef d of
          TStruct {} -> vcat' [ cUnboxedProd d, generateUnboxedMethods d ]
          TUnion {}  -> vcat' [ cSumTags [d]
                              , cUnboxedSum d
                              , generateUnboxedMethods d
                              ]

      MutRec ds ->
        vcat' $
          -- 1. Declare names of types
          [ vcat' $ map cTypeDecl $ sums ++ prods
          , sumWrapper $ vcat' $ map cTypeDecl sums
          ] ++

          -- 2. Declare tags
          [ cSumTags sums ] ++

          -- 3. Declare boxed sums
          map cBoxedSum sums ++

          -- 4. Declare products
          map cUnboxedProd prods ++

          -- 5. Declare unboxed sums
          [ sumWrapper $ vcat' $ map cUnboxedSum sums ] ++

          -- 6. Generate methods
          [ sumWrapper (vcat $ map generateUnboxedMethods sums) ] ++
          map generateUnboxedMethods prods ++
          map generateBoxedMethods sums

        where
        (sums,prods) = orderRecGroup ds
        sumWrapper d
          | null sums = d
          | otherwise = "namespace DDL { namespace User {" $$ nest 2 d $$ "}}"




{- Note: Product types shouldh't be directly recursive as that would
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
    (withou enums)
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

  sumSet  = Set.fromList (map tName sums)
  deps td = (tName td, freeTCons td `Set.difference` sumSet)




cTypeDecl :: TDecl -> CDecl
cTypeDecl ty = cTypeDecl' ty <.> semi

-- Note: this does not add the semi at the end, so we can reuse it in `Def`.
cTypeDecl' :: TDecl -> CDecl
cTypeDecl' ty =
  vcat [ cTemplateDecl ty, "class" <+> cTName (tName ty) ]

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


-- | Signature for the @copy@ method
copyMethod :: Doc
copyMethod = "void" <+> cCall "copy" []

-- | Signature for the @free@ method
freeMethod :: Doc
freeMethod = "void" <+> cCall "free" []

-- | Interface definition for struct types
cUnboxedProd :: TDecl -> CDecl
cUnboxedProd ty =
  vcat
    [ cTypeDecl' ty <+> ": public DDL::HasRefs {"
    , nest 2 $ vcat $ map cStmt attribs
    , "public:"
    , nest 2 $ vcat $ map cStmt methods
    , "};"
    ]
  where
  TStruct fields = tDef ty
  labs    = map (cLabel   . fst) fields
  tys     = map (cSemType . snd) fields
  attribs = [ cSemType t <+> cField n | ((_,t),n) <- zip fields [ 0 .. ]
                                      , t /= TUnit  ]
  methods =
       [ "// Construcotr" ]
    ++ [ "void" <+> cCall "init" tys ]
    ++ [ "// Selectors" ]
    ++ [ t <+> cCall nm [] | (f,t) <- zip labs tys
                           , pref  <- [ "get", "borrow" ]
                           , let nm = pref <.> "_" <.> f
       ]
    ++ [ "// Memory Management" ]
    ++ [ copyMethod, freeMethod ]
  -- XXX: Equality



--------------------------------------------------------------------------------
-- Tags

-- | Declare a bunch of tag types
cSumTags :: [TDecl] -> CDecl
cSumTags sums
  | null sums = empty
  | otherwise = vcat
                  [ "namespace Tag {"
                  , nest 2 $ vcat $ map cSumTag sums
                  , "}"
                  ]

-- | The enum for a tag type
cSumTag :: TDecl -> CDecl
cSumTag ty =
  fsep $ [ "enum", cTName (tName ty), "{" ] ++
         punctuate comma (map (cLabel . fst) fields) ++
         [ "};" ]
  where
  TUnion fields = tDef ty

-- | The type of tags for the given type
cSumTagT :: TDecl -> CType
cSumTagT tdecl = "Tag::" <.> cTName (tName tdecl)

cSumTagV :: Label -> Doc
cSumTagV l = "Tag::" <.> cLabel l

-- | @getTag@ method signature
cSumGetTag :: TDecl -> Doc
cSumGetTag td = cSumTagT td <+> cCall "getTag" []
--------------------------------------------------------------------------------


-- | Signatures for getters of a sum
cSumGetters :: TDecl -> [ Doc ]
cSumGetters tdecl =
  [ cSemType t <+> cCall (pref <.> "_" <.> cLabel l) []
  | (l,t) <- getFields tdecl
  , pref <- ["get","borrow"]
  ]

-- | Signatures for constructors of a sum
cSumCtrs :: TDecl -> [Doc]
cSumCtrs tdecl =
  [ "void" <+> cCall ("init_" <.> cLabel l)
                     [ cSemType ty | ty <- [t], t /= TUnit ]
  | (l,t) <- getFields tdecl
  ]

-- | Class signature for an unboxed sum
cUnboxedSum :: TDecl -> CDecl
cUnboxedSum tdecl =
  vcat
    [ cTypeDecl' tdecl <+> ": DDL::HasRefs {"
    , nest 2 $ vcat attribs
    , "public:"
    , nest 2 $ vcat $ map cStmt methods
    , "};"
    ]
  where
  fields = getFields tdecl

  attribs =
    [ cStmt $ cSumTagT tdecl <+> "tag" ] ++
    [ cStmt $ vcat
        [ "union Data {"
        , nest 2 $ vcat $ map cStmt [ cSemType t <+> cField n | (t,n) <- fs ]
        , nest 2 "Data() {}"
        , "} data"
        ]
    | let fs = [ (t,n) | ((_,t),n) <- zip fields [ 0 .. ], t /= TUnit ]
    , not (null fs)
    ]

  methods =
      [ "// Construcotrs" ]
   ++ cSumCtrs tdecl
   ++ [ "// Selectors" ]
   ++ [ cSumGetTag tdecl ]
   ++ cSumGetters tdecl
   ++ [ "// Memory Management" ]
   ++ [ copyMethod, freeMethod ]
  -- XXX: Equality


-- | Class signature for a boxed sum
cBoxedSum :: TDecl -> CDecl
cBoxedSum tdecl =
  vcat
    [ cTypeDecl' tdecl <+> ": public" <+> cInst "DDL::Boxed" [tuse] <+> "{"
    , "public:"
    , nest 2 $ vcat $ map cStmt methods
    , "};"
    ]
  where
  methods =
       [ "// Construcotrs" ]
    ++ cSumCtrs tdecl
    ++ [ "// Selectors" ]
    ++ [ cSumGetTag tdecl ]
    ++ cSumGetters tdecl

  tuse = "DDL::User::" <.> cSemType (TUser ut)
  ut = UserType
         { utName    = tName tdecl
         , utNumArgs = map TSizeParam (tTParamKNumber tdecl)
         , utTyArgs  = map TParam (tTParamKValue tdecl)
         }


--------------------------------------------------------------------------------
-- Method definitions

data GenBoxed = GenBoxed  | GenUnboxed
data GenOwn   = GenBorrow | GenOwn

generateBoxedMethods :: TDecl -> Doc
generateBoxedMethods ty =
  vcat' $
    [ "// --- Methods for" <+> pp (tName ty) <+> "------------------" ] ++
    defCons      GenBoxed ty ++
    defSelectors GenBoxed ty

generateUnboxedMethods :: TDecl -> Doc
generateUnboxedMethods ty =
  vcat' $
    [ "// --- Methods for" <+> pp (tName ty) <+> "------------------"
    , defCopyFree "copy" ty
    , defCopyFree "free" ty
    ] ++
    defCons      GenUnboxed ty ++
    defSelectors GenUnboxed ty ++
    [defEq ty]

defMethod :: TDecl -> CType -> Doc -> [Doc] -> [CStmt] -> CDecl
defMethod tdecl retT fun params def =
  vcat [ cTemplateDecl tdecl
       , "inline"
       , retT <+> cCall name params <+> "{"
       , nest 2 (vcat def)
       , "}"
       ]
  where
  name = cTName (tName tdecl) <.> "::" <.> fun

--------------------------------------------------------------------------------
-- Equality


defEq :: TDecl -> CDecl
defEq tdecl =
  defMethod tdecl "bool" "operator ==" [ cTName (tName tdecl) <+> "x" ]
  case tDef tdecl of
    TStruct fs ->
      [ "if" <+> parens (f <+> "!=" <+> "x." <.> f) <+> cStmt "return false"
      | ((_,t),n) <- fs `zip` [ 0 .. ]
      , let f = cField n
      , t /= TUnit
      ] ++
      [ cStmt "return true" ]
    TUnion fs ->
      [ "if (tag != x.tag) return false;"
      , "switch (tag) {"
      , "// TODO"
      , "}"
      , "return false; // unreachable"
      ]

--------------------------------------------------------------------------------
-- Construcotrs

defCons :: GenBoxed -> TDecl -> [CDecl]
defCons boxed tdecl =
  case tDef tdecl of
    TStruct {} -> [ defStructCon boxed tdecl ]
    TUnion  {} -> defUnionCons boxed tdecl


defStructCon :: GenBoxed -> TDecl -> CDecl
defStructCon boxed tdecl = defMethod tdecl "void" "init" params def
  where
  params = [ t <+> x | (t,x,_) <- fs ]
  def =
    case boxed of
      GenBoxed   -> [ cStmt $ cCall "getValue().init" [ x | (_,x,_) <- fs ] ]
      GenUnboxed -> [ cStmt (f <+> "=" <+> x) | (_,x,f) <- fs ]

  fs = [ (cSemType t, cLabel l, cField n)
       | ((l,t),n) <- getFields tdecl `zip` [ 0 .. ]
       , t /= TUnit
       ]

defUnionCons :: GenBoxed -> TDecl -> [CDecl]
defUnionCons boxed tdecl = zipWith defCon (getFields tdecl) [ 0 .. ]
  where
  defCon (l,t) n =
    let name = "init_" <.> cLabel l
        fs   = [ (cSemType t, cLabel l) | t /= TUnit ]
    in defMethod tdecl "void" name [ ty <+> x | (ty,x) <- fs ]
       case boxed of
         GenBoxed -> [ cStmt (cCall ("getValue()." <.> name) (map snd fs)) ]
         GenUnboxed ->
            cStmt ("tag =" <+> cSumTagV l)
          : [ cStmt ("data." <.> cField n <+> "=" <+> cLabel l) | t /= TUnit ]


--------------------------------------------------------------------------------
-- Selectors


defSelectors :: GenBoxed -> TDecl -> [CDecl]
defSelectors boxed tdecl =
  concatMap (defSelectorsOwn boxed tdecl) [GenBorrow,GenOwn]

-- XXX: Maybe some selectors should return a reference?
defSelectorsOwn :: GenBoxed -> TDecl -> GenOwn -> [CDecl]
defSelectorsOwn boxed tdecl borrow = zipWith sel (getFields tdecl) [ 0 .. ]
  where
  pref = case borrow of
           GenBorrow -> "borrow"
           GenOwn    -> "get"
  uni  = case tDef tdecl of
           TStruct _ -> False
           TUnion  _ -> True
  sel (l,t) n =
    let name = pref <.> "_" <.> cLabel l
    in defMethod tdecl (cSemType t) name []
       case boxed of
         GenBoxed -> [ cStmt (cCall ("getValue()." <.> name) []) ]
         GenUnboxed ->
           [ s | GenOwn <- [borrow], Just s <- [ maybeCopyFree "copy" t f ] ] ++
           [ cStmt $ "return" <+>
                     case t of
                       TUnit -> "DDL::Unit()"
                       _     -> f
           ]
           where f = if uni then "data." <.> cField n else cField n


--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Copy & Free

-- | Define a copy/free method for the given type
defCopyFree :: Doc -> TDecl -> CDecl
defCopyFree fun tdecl = defMethod tdecl "void" fun [] def
  where
  def =
    case tDef tdecl of
      TStruct _ -> map snd (stmts True)
      TUnion _ ->
        case stmts False of
          [] -> []
          xs -> [ vcat [ "switch" <+> parens (cCall "getTag" []) <+> "{"
                       , nest 2 $ vcat' [ "case" <+> cSumTagV l <.> colon $$
                                           nest 2 (s $$ "break;")
                                        | (l,s) <- xs ]
                       , "}"
                       ]
                ]

  stmts struct =
    let dat = if struct then empty else "data."
    in
    catMaybes
       [ do s <- maybeCopyFree fun t (dat <.> cField n)
            pure (l,s)

       | ((l,t),n) <- getFields tdecl `zip` [ 0.. ]
       ]


-- | Emit some code to copy/free a field
maybeCopyFree :: Doc -> Type -> CExpr -> Maybe CStmt
maybeCopyFree fun ty e =
  case ty of
    TParam {} ->
      Just $ vcat [ "if constexpr" <+> parens check <+> "{", nest 2 doIt , "}" ]
      where
      check = cInst "std::is_base_of" [ "DDL::HasRefs", cSemType ty ]
                                                                <.> "::value"

    _ -> case typeRep ty of
           HasRefs -> Just doIt
           NoRefs  -> Nothing

  where
  doIt = cStmt (e <.> "." <.> cCall fun [])



