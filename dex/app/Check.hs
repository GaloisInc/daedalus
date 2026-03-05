module Check (
  checkModule,
  runValidator,
  ValidationError(..),
  Mismatch(..),
  DDLTypes(..),
  Check
) where


import Control.Monad
import Data.Set(Set)
import Data.Set qualified as Set
import Data.Map(Map)
import Data.Map qualified as Map
import Data.Maybe(isJust)
import Data.Text(Text)
import Data.Text qualified as Text

import AlexTools

import Daedalus.PP
import Daedalus.Core qualified as Core

import Monad
import Subst
import Name
import Type
import AST


type Check = M RO RW ValidationError

data ValidationError =
    UndefinedName (Loc PName)
  | AmbiguousName (Loc PName) Name Name  -- ^ modules containing conflicting definitions
  | MultipleDefinitions Name SourceRange SourceRange
  | MalformedType (Loc PName) (Mismatch Int) (Mismatch Int) -- value, size
  | InvalidTParam (Loc Name) (Mismatch TParamFlavor)
  | TParamShadowBuiltIn (Loc Name)
  | UnusedTParam (Loc Name)
  | AlreadyExported (Loc Name) SourceRange
  | InvalidSelector (Type DDLTCon) (Loc Name)
  | CannotFindDefault SourceRange (Type DDLTCon)
  | DefaultWithParams (Loc PName)
  | InvalidParams (Loc PName) (Mismatch Int)
  | MalformedExporter (Loc PName) (Mismatch Int)
  | DDLTypeMismatch (Loc PName) (Mismatch [Type DDLTCon])
  | ForeignTypeMismatch (Loc PName) (Mismatch (Type QName))
  | InvalidCaseType (Loc Name) (Type DDLTCon)
  | InvalidCon (Loc Name)
  | MissingCase (Loc Name) Text
  | VarNotExported (Loc Name) [Text]
  | InvalidForType (Loc Name) (Type DDLTCon)
  | InvalidForBinders (Loc Name) (Mismatch Int)
  

data Mismatch a = Mismatch {
  expected :: a,
  actual   :: a
}

instance PP ValidationError where
  pp err =
    case err of
      UndefinedName x -> msg x \d -> ["Undefined name" <+> d]
      AmbiguousName x a b ->
        msg x \d ->
          [ "Ambiguous name" <+> d
          , "It may refer to the definition in" <+> qu a <+> ", or" <+> qu b
          ]
      MultipleDefinitions x r1 r2 ->
        vcat
          [ "Multiple definitions for" <+> pp x
          , nest 2 ("defined at" <+> text (prettySourcePos (sourceFrom r1)))
          , nest 2 ("and also at" <+> text (prettySourcePos (sourceFrom r2)))
          ]
      MalformedType x v s ->
        msg x \d ->
          ("Malformed type" <+> d) :
          mis v "value arguments" ++ mis s "numeric arguments"

      InvalidTParam x a ->
          msg x \d ->
          ("Invalid type parameter" <+> d) : mis a ""

      TParamShadowBuiltIn x ->
        msg x \d ->
          [ "Type parameter" <+> d <+> "shadows a built-in type" ]

      UnusedTParam x ->
        msg x \d ->
          [ "Type parameter" <+> d <+> "is not used in the exporter's type" ]

      
      AlreadyExported x y ->
        msg x \d ->
          [ "Variable" <+> d <+> "was already exported."
          , nest 2 ("Other export:" <+> ppLoc y)
          ]

      InvalidSelector t l ->
        msg l \d ->
          [ "Type" <+> backticks (pp t) <+>
            "does not have selector" <+> d ]

      CannotFindDefault rng ty ->
        ppLLoc rng <+>
          "Cannot determine a default exporter for type" <+> backticks (pp ty)
      
      DefaultWithParams x ->
        msg x \d ->
          ["Exporter" <+> d <+> "is declared as `default`, but it has exporter parameters."]

      MalformedExporter x m ->
        msg x \d ->
          ("Incorrect number of arguments to" <+> d <.> ":") : mis m ""

      DDLTypeMismatch x m ->
        msg x \d ->
          ("Type error at" <+> d <.> ":") :
            mis' m
                 Mismatch { expected = commaSep (map pp (expected m)),
                            actual   = commaSep (map pp (actual m)) }

      ForeignTypeMismatch x m ->
        msg x \d ->
          ("Type error at" <+> d <.> ":") : mis m ""

      InvalidCaseType x t ->
        msg x \d ->
          [ "Cannot case on" <+> d <.> ".",
            "It has type:" <+> backticks (pp t) 
          ]

      InvalidCon l ->
        msg l \d ->
          [ "Invalid case alternative:" <+> d <.> "."]

      MissingCase x l ->
        msg x \d ->
          [ "Incomplete cases for" <+> d <.> ":",
            "Missing case for" <+> backticks (pp l)
          ]

      VarNotExported x ls ->
        ppLoc (locRange x) <+>
          let
            what = if null ls then "Variable" else "Field"
            thing = qu (foldl (\doc l -> doc <.> "." <.> pp l) (pp (locThing x)) ls)
          in what <+> thing <+> "was never exported."
        
      InvalidForType x t ->
        msg x \d ->
          [ d <+> "does not support iteration.",
            "It has type:" <+> backticks (pp t)
          ]

      InvalidForBinders x m ->
        msg x \d ->
          ("Incorrect binders when iterating over" <+> d <.> ":") : mis m "binders"

      InvalidParams x m ->
        msg x \d ->
          ("Incorrect number of parameters in the definitions of" <+> d)
          : mis m "parameters"

    where
    ppLoc = text . prettySourcePos . sourceFrom
    ppLLoc = text . prettySourcePosLong . sourceFrom
    mis' m1 m2 =
      if expected m1 == actual m1
        then []
        else [ "Expected:" <+> expected m2,
               "  Actual:" <+> actual m2 ]

    mis m suf =
      mis'
        m
        Mismatch { expected = pp (expected m) <+> suf,
                   actual = pp (actual m) <+> suf }
      
    msg x f =
      let pre = ppLLoc (locRange x) in
      case f (qu (locThing x)) of
        [] -> pre
        a : as -> (pre <+> a) $$ nest 2 (vcat as)
      
    qu a = backticks (pp a)

data DDLTypes = DDLTypes {
  ddlTypesByTName       :: Map Core.TName Core.TDecl,
  ddlTypesByName        :: Map Name [(Name,Core.TDecl)],
  ddlTypesByModule      :: Map Name (Map Name Core.TDecl)
}

data ExporterTypes = ExporterTypes {
  exportersByName       :: Map Name [(Name,ExporterType DDLTCon QName)],
  exportersByModule     :: Map Name (Map Name (ExporterType DDLTCon QName)),
  exporterDefaults      :: [(Type DDLTCon,(QName, ExporterType DDLTCon QName))]
}

data RO = RO {
  ddlTypes              :: DDLTypes,
  -- ^ Deadalus type declarations

  patternVars           :: Maybe (Map Name (Name, Name))
  -- ^ If this is `Just` we desugar pattern variables to union selectors

}

data RW = RW {
  curModule             :: Name,

  foreignTypesByName    :: Map Name [(Name,ForeignTypeDecl)],
  foreignTypesByModule  :: Map Name (Map Name ForeignTypeDecl),
  exporterTypes         :: ExporterTypes,
  
  tvarSeed :: !Int,
  -- ^ Used to generate fresh foreign unification variables.

  -- Local to a function
  tparamStatus  :: Map Name (Maybe TParamFlavor),
  -- ^ Local type parameters. These change during signature validation,
  -- which is when we infer if the kind of parameter (Daedalus vs. foreign).
  -- They don't change during declaration validation.

  varTypes      :: Map Name (SourceRange, VarStatus),
  -- ^ Type of Daedalus variables, together with their export status
  -- for linearity validation.

  localExporters  :: Map Name (BasicExporterType DDLTCon QName)
  -- ^ Types of local export functions.  These don't really change
  -- but are here for consitency with the rest.
}

-- | Status of a local variable
data VarStatus =
    NotExported (Type DDLTCon)
  | ExportedBy (Loc Name)
  | ExplodedBy (Type DDLTCon) (Loc Name) (Map Core.Label VarStatus)

-- | Indicates what kind of type parameter we have
data TParamFlavor = TParamDDL | TParamForeign
  deriving Eq

instance PP TParamFlavor where
  pp f =
    case f of
      TParamDDL -> "a Daedalus type"
      TParamForeign -> "a foreign type"

-- | Generate a fresh unification variable
freshVar :: Check Name
freshVar =
  do
    rw <- getState
    let n  = tvarSeed rw
        nm = nameFromText ("fresh-" <> Text.pack (show n))
    setState rw { tvarSeed = n + 1 }
    pure nm


-- | Check if the given name is a type parameter of the given kind.
isTParam :: Loc Name -> TParamFlavor -> Check Bool
isTParam x want =
  do
    rw <- getState
    let x' = locThing x
    case Map.lookup x' (tparamStatus rw) of
      Nothing -> pure False
      Just status ->
        case status of
          Nothing ->
            do
              setState rw { tparamStatus =
                              Map.insert x' (Just want) (tparamStatus rw) }
              pure True
          Just s
            | s == want -> pure True
            | otherwise -> reportError (InvalidTParam x (Mismatch want s))
                  


-- | Check a type alias. Adds it to the state.
checkTypeAlias :: ForeignTypeDecl -> Check ()
checkTypeAlias decl =
  do
    checkUnique (ftParams decl)
    mapM_ checkValidParam (ftDef decl)
    mo <- curModule <$> getState
    let nm = locThing (ftName decl)
    updState \rw -> rw {
      foreignTypesByName = Map.insertWith (++) nm [(mo,decl)] (foreignTypesByName rw),
      foreignTypesByModule = Map.insertWith Map.union mo (Map.singleton nm decl) (foreignTypesByModule rw)
    }
    where
    ps = Set.fromList (map locThing (ftParams decl))
    checkValidParam x =
      unless (locThing x `Set.member` ps)
        (reportError (UndefinedName (Unqual <$> x)))
      
     
-- | Check that a bunch of parameters are distinct.
checkUnique :: [Loc Name] -> Check ()
checkUnique xs =
  case Map.minView bad of
    Just (err, _) -> reportError err
    _ -> pure ()
  where
  bad = Map.mapMaybeWithKey check
      $ Map.fromListWith (++) [ (locThing x, [locRange x]) | x <- xs ]
  check k ys =
    case ys of
      a : b : _ -> Just (MultipleDefinitions k a b)
      _ -> Nothing


-- | Validate and resolve a foreign type.
checkForeignType :: Type PName -> Check (Type QName)
checkForeignType ty =
  case ty of
    Type nm ts [] ->
      do
        args <- mapM checkForeignType ts
        rw   <- getState
        case locThing nm of

          Unqual x ->
            do
              let nm' = nm { locThing = x }
              isTP <- isTParam nm' TParamForeign
              if isTP
                then pure (TVar nm')
                else
                  do
                    case Map.lookup x (foreignTypesByName rw) of
                      Nothing -> reportError (UndefinedName nm)
                      Just [] -> error "[BUG] `checkForeignType` []"
                      Just [(m,def)] ->
                        do
                          let q = QName { qName = x, qModule = m }
                          mkTCApp def q nm args
                      Just ((m1,_) : (m2,_) : _) ->
                        reportError (AmbiguousName nm m1 m2)
          Qual q ->
            do
              let m = qModule q
                  n = qName q
              case Map.lookup n =<< Map.lookup m (foreignTypesByModule rw) of
                Nothing  -> reportError (UndefinedName nm)
                Just def -> mkTCApp def q nm args

    _ -> error "[BUG] `checkForeignType`"

  where
  mkTCApp def q nm args =
    do
      let have = length args
          need = length (ftParams def)
      unless (have == need)
        (reportError (MalformedType nm (Mismatch need have) (Mismatch 0 0)))
      pure (Type nm { locThing = q } args [])


-- | Names of Daedalus type constructors
ddlTCons :: Map Name (DDLTCon,Int,Int)
ddlTCons = Map.fromList
  [ 
    "bool"    ~> (TBool,0,0),
    "double"  ~> (TDouble,0,0),
    "float"   ~> (TFloat,0,0),
    "stream"  ~> (TStream,0,0),
    "int"     ~> (TInteger,0,0),
    "uint"    ~> (TUInt,0,1),
    "sint"    ~> (TSInt,0,1),
    "maybe"   ~> (TMaybe,1,0),
    "builder" ~> (TBuilder,1,0),
    "[]"      ~> (TArray,1,0),
    "[:]"     ~> (TMap,2,0)
  ]
  where x ~> y = (nameFromText x, y)


-- | Validate and resolve a Daedalus type
checkDDLType :: Type PName -> Check (Type DDLTCon)
checkDDLType ty =
  case ty of
    Type nm ts szs ->
      do
        args <- mapM checkDDLType ts

        case locThing nm of

          Unqual x ->
            do
              let nm' = nm { locThing = x }
              isTP <- isTParam nm' TParamDDL
              if isTP
                then
                  do
                    when (x `Map.member` ddlTCons)
                      (reportError (TParamShadowBuiltIn nm'))
                    pure (TVar nm')
                else
                  case Map.lookup x ddlTCons of
                    Just (tc,vs,ss) -> mkTCApp nm tc vs ss args szs
                    Nothing ->
                      do
                        env <- getEnv
                        case Map.lookup x (ddlTypesByName (ddlTypes env)) of
                          Nothing -> reportError (UndefinedName nm)
                          Just [] -> error "[BUG] `checkDDLType` []"
                          Just [(_,def)] -> mkTCUserApp def nm args szs
                          Just ((m1,_) : (m2,_) : _) ->
                            reportError (AmbiguousName nm m1 m2)
          Qual q ->
            do
              let m = qModule q
                  n = qName q
              env <- getEnv
              case Map.lookup n =<< Map.lookup m (ddlTypesByModule (ddlTypes env)) of
                Nothing  -> reportError (UndefinedName nm)
                Just def -> mkTCUserApp def nm args szs

    _ -> error "[BUG] `checkDDLType`"

  where
  mkTCUserApp def nm =
    mkTCApp nm (TUser (Core.tName def))
      (length (Core.tTParamKValue def))
      (length (Core.tTParamKNumber def))
    
  mkTCApp nm con need_val need_sz args szs =
    do
      let have_val = length args
          have_sz  = length szs
      unless (have_val == need_val && have_sz == need_sz)
        (reportError (MalformedType nm (Mismatch need_val have_val)
                                       (Mismatch need_sz have_sz)))
      pure (Type nm { locThing = con } args szs)

checkExporterType ::
  BasicExporterType PName PName -> Check (BasicExporterType DDLTCon QName)
checkExporterType (a :-> b) =
  do
    a' <- mapM checkDDLType a
    b' <- checkForeignType b
    pure (a' :-> b')

-- | Validate the type of a declaration. Add it to the state
checkDeclSig :: Decl PName PName -> Check ()
checkDeclSig d =
  do
    let tps   = declDDLTParams d
        tpMap = Map.fromList [ (locThing tp, tp) | tp <- tps ]
    checkUnique tps
    updState \rw -> rw { tparamStatus = const Nothing <$> tpMap }
    expT <- checkExporterType (declType d)

    -- We require that all type parameters are mentioned in the type of
    -- the exporter (not counting exporter arguments).  This ensures that
    -- we can automatically infer the types at use sites.
    doneTPs <- tparamStatus <$> getState
    case [ UnusedTParam (tpMap Map.! x) | (x,Nothing) <- Map.toList doneTPs ] of
      x : _ -> reportError x
      [] -> pure ()

    expParams <- mapM (checkExporterType . snd) (declFunParams d)                  

    mo <- curModule <$> getState
    let nm = fromU (locThing (declName d))

    let ty = Forall {
      etDDLTypeVars =
        [ locThing tp
        | tp <- tps,
          Map.lookup (locThing tp) doneTPs == Just (Just TParamDDL)
        ],
      etForeignTypeVars =
        [ locThing tp
        | tp <- tps,
          Map.lookup (locThing tp) doneTPs == Just (Just TParamForeign)
        ],
      etExporterParams = expParams,
      etType = expT
    }
    
    let argTy :-> _ = expT
    dflt <-
      if declDefault d
        then
          do
            unless (null expParams) (reportError (DefaultWithParams (declName d)))
            case argTy of
              [t] | null expParams -> pure (Just t)
              n -> reportError (InvalidParams (declName d) Mismatch { expected = 1, actual = length n })
        else pure Nothing

    updState \rw ->
      let es = exporterTypes rw
      in rw {
        exporterTypes = ExporterTypes {
          exportersByName = Map.insertWith (++) nm [(mo,ty)] (exportersByName es),
          exportersByModule =
            Map.insertWith Map.union mo (Map.singleton nm ty) (exportersByModule es),
          exporterDefaults =
            case dflt of
              Just t ->
                (t, (QName { qModule = mo, qName = nm },ty)) : exporterDefaults es
              Nothing -> exporterDefaults es
        }
    }


checkDecl :: Decl PName PName -> Check (Decl DDLTCon QName)
checkDecl d =
  do
    rw <- getState
    let ourMod  = curModule rw
        ourName = fromU (locThing (declName d))
        exTs = exporterTypes rw
        ourType =
          case Map.lookup ourName =<< Map.lookup ourMod (exportersByModule exTs) of
            Just ty -> ty
            Nothing -> error "[BUG] `checkDecl` no type"
        argTy :-> _ = etType ourType
        tpStatus x =
          if locThing x `elem` etDDLTypeVars ourType
            then TParamDDL
            else TParamForeign
        
    -- setup local variables
    setState rw {
      tparamStatus =
        Map.fromList
          [ (locThing tp,Just (tpStatus tp)) | tp <- declDDLTParams d ],
     
      localExporters =
        Map.fromList
            (map (locThing . fst) (declFunParams d) `zip` etExporterParams ourType)
    }

    let ty@(_ :-> resT) = etType ourType
  
    -- Validate body
    def <-
      case declDef d of
        DeclExtern -> pure DeclExtern
        d1 -> foldr (uncurry withVar) (checkDeclDef (declName d) (length argTy) d1 (undefined,resT))
                    (declArg d `zip` argTy)
                  

    pure Decl {
      declDefault = declDefault d,
      declName    = (declName d) { locThing = QName { qModule = ourMod, qName = ourName } },
      declDDLTParams = filter ((TParamDDL ==) . tpStatus) (declDDLTParams d),
      declForeignTParams = filter ((TParamForeign ==) . tpStatus) (declDDLTParams d),
      declFunParams =
        [ (x,t) | ((x,_),t) <- declFunParams d `zip` etExporterParams ourType ],
      declArg = declArg d,
      declType = ty,
      declDef = def      
    }

fromU :: PName -> Name
fromU x =
  case x of
    Unqual y -> y
    Qual {} -> error "[BUG] `fromU` Qual"

type ResT = (Set Name, Type QName) -- Skolem variables, expected type

-- | Check the definition of an exporter    
checkDeclDef :: Loc PName -> Int -> DeclDef PName PName -> ResT -> Check (DeclDef DDLTCon QName)
checkDeclDef nm argNum def resT =
  case def of
    DeclDef code -> DeclDef <$> checkForeignCode code (Just resT)
    DeclCase x as -> checkOne >> (DeclCase x <$> checkCase resT x as)
    DeclLoop loop -> checkOne >> (DeclLoop <$> checkLoop loop)
    DeclExtern -> pure DeclExtern
  where
  checkOne
    | argNum == 1 = pure ()
    | otherwise = reportError (InvalidParams nm Mismatch { expected = 1, actual = argNum })


-- | Validate a case exporter
checkCase ::
  ResT ->
  Loc Name ->
  [(Pat PName, ForeignCode PName PName)] ->
  Check [(Pat DDLTCon,ForeignCode DDLTCon QName)]
checkCase resT x alts =
  do
    ty <- checkDDLVar x []
    case ty of

      Type tc@Loc { locThing = TBool } [] [] ->
        let u = Type tc { locThing = TUnit } [] [] in 
        checkAlts resT x [("false", u), ("true", u)] alts

      Type tc@Loc { locThing = TMaybe } [f] [] ->
        let u = Type tc { locThing = TUnit } [] [] in 
        checkAlts resT x [("nothing", u), ("just", f)] alts

      Type tc@Loc { locThing = TUser ut } vargs nargs ->
        do
          env <- getEnv
          case Map.lookup ut (ddlTypesByTName (ddlTypes env)) of
            Nothing -> error "[BUG] `checkCase` Missing type"
            Just tdecl ->
              case Core.tDef tdecl of
                Core.TUnion opts -> checkAlts resT x (map imp opts) alts
                  where
                  suV = Map.fromList (zip (Core.tTParamKValue tdecl) vargs)
                  nuV = Map.fromList (zip (Core.tTParamKNumber tdecl) nargs)
                  imp (con,y) = (con, coreTypeToType (locRange tc) suV nuV y)
                Core.TBitdata {} -> error "XXX: `checkCase` `bitdata`"
                Core.TStruct {} -> reportError (InvalidCaseType x ty)

      _ -> reportError (InvalidCaseType x ty)

-- | Validate the alternatives of a case exporter
checkAlts ::
  ResT ->
  Loc Name -> [(Text,Type DDLTCon)] -> [(Pat PName, ForeignCode PName PName)] ->
  Check [(Pat DDLTCon,ForeignCode DDLTCon QName)]
checkAlts resT disc needList = checkAll (Map.fromList needList) []
  where
  checkAll mp done pats =
    case pats of
      [] ->
        case Map.minViewWithKey mp of
          Nothing -> pure (reverse done)
          Just ((k,_), _) -> reportError (MissingCase disc k)
      p@(PCon nm _, _) : more ->
        do
          p' <- check mp p
          checkAll (Map.delete (nameToText (locThing nm)) mp) (p':done) more


  check mp (PCon nm mbX,code) =
    let lab = nameToText (locThing nm)
    in
    case Map.lookup lab mp of
      Nothing -> reportError (InvalidCon nm)
      Just ty ->
        case (ty, mbX) of
          (Type Loc { locThing = TUnit } [] [], Nothing) ->
            do
              rhs <- checkForeignCode code (Just resT)
              pure (PCon nm Nothing, rhs)
          (_, Nothing) -> reportError (VarNotExported disc [lab])
          (ft, Just (f,_)) ->
            withCaseVar f ft (locThing disc, locThing nm)
            do
              code' <- checkForeignCode code (Just resT)
              pure (PCon nm (Just (f,Just ft)), code')


-- | Validate a loop exporter
checkLoop :: Loop PName PName -> Check (Loop DDLTCon QName)
checkLoop l =
  do
    mapM_ checkParam (loopInit l)
    let (vs,xs,body) = loopFor l
    ty <- checkDDLVar xs []
    body' <-
      case ty of
        Type Loc { locThing = TArray } [a] [] -> checkBody xs vs [a] body
        Type Loc { locThing = TMap } [k,v] [] -> checkBody xs vs [k,v] body
        -- Type Loc { locThing = TBuilder } [a] [] -> checkBody xs vs [a] body
        _ -> reportError (InvalidForType xs ty)
    pure l { loopFor = (vs,xs,body') }
  where
  checkParam x =
    do
      tps <- tparamStatus <$> getState
      case Map.lookup (locThing x) tps of
        Just (Just TParamForeign) -> pure ()
        Just (Just TParamDDL) ->
          reportError (InvalidTParam x (Mismatch TParamForeign TParamDDL))
        Just Nothing -> error "[BUG] `checkLoop` checkParam"
        Nothing -> reportError (UndefinedName (Unqual <$> x))
      
  checkBody xs vs ts body
    | have /= need = reportError (InvalidForBinders xs (Mismatch need have))
    | Just ((a,(r1,r2)),_) <- repeated =
      reportError (MultipleDefinitions a r1 r2)
    | otherwise =
      foldr (uncurry withVar) (checkForeignCode body Nothing) (zip vs ts)
        where
        have = length vs
        need = length ts
        repeated = Map.minViewWithKey
                 $ Map.mapMaybe mult
                 $ Map.fromListWith (++) [ (locThing x,[locRange x]) | x <- vs ]
        mult m =
          case m of
            a : b : _ -> Just (a,b)
            _ -> Nothing



-- | Validate a piece of foreign code with exporter escapes.
checkForeignCode :: ForeignCode PName PName -> Maybe ResT -> Check (ForeignCode DDLTCon QName)
checkForeignCode code expectedT =
  case code of
    Direct e -> Direct <$> checkExportExpr e expectedT
    Splice q -> Splice <$> mapM checkForeignCodeSplice q

checkForeignCodeSplice ::
  ForeignCodeSplice PName PName -> Check (ForeignCodeSplice DDLTCon QName)
checkForeignCodeSplice spl =
  case spl of
    SpliceCode e ->
      case (exportWith e, exportExpr e) of
        (Nothing, [DDLVar x]) ->
          do
            tps <- tparamStatus <$> getState
            case Map.lookup (locThing x) tps of
              Just (Just TParamForeign) -> pure (SpliceTParam x)
              _ -> SpliceCode <$> checkExportExpr e Nothing
        _ -> SpliceCode <$> checkExportExpr e Nothing
    SpliceTParam {} -> error "[BUG] checkForeignCodeSplice: TParam"


-- | Validate the names and types in an exporter.
checkExportExpr :: ExportExpr PName PName -> Maybe ResT -> Check (ExportExpr DDLTCon QName)
checkExportExpr ex expectedT =
  do
    (arg, t) <- mapAndUnzipM checkDDLExpr (exportExpr ex)
    (skolem,rt) <-
      case expectedT of
        Nothing ->
          do
            r <- freshVar
            pure (Set.empty, TVar Loc { locRange = getRange ex, locThing = r })
        Just rt -> pure rt
    e <- 
      case exportWith ex of
        Nothing
          | [argT] <- t ->
          do
            let rng = getRange ex
            (q,ty) <- findDefault rng argT
            let nm  = Loc { locRange = rng, locThing = q }
            pure (Qual <$> nm, \as bs cs et -> ExportTop nm as bs cs (Just et), ty, [])
          | otherwise -> error "[BUG] checkExportExpr: default but not 1 arg"
        Just e  -> resolveExporterFun e
    (e', su) <- checkExporter' skolem e (t :-> rt)
    let e''  = apForeignSubstExp su e'
        resT = apSubstT su rt
    pure ExportExpr {
      exportWith = Just e'',
      exportExpr = arg,
      exportResult = Just resT
    }

type ExporterFun =
  ( Loc PName,
    -- Name of exporter for error reporting

    [Type DDLTCon] -> [Type QName] -> [Exporter DDLTCon QName] -> BasicExporterType DDLTCon QName -> Exporter DDLTCon QName,
    -- Use this to construct the exporter

    ExporterType DDLTCon QName,
    -- This is the type of the exporter

    [Exporter PName PName]
    -- Arguments to the exporter
  )

  
-- | Figure out which exporter function we are talking about
-- (i.e., is it local or top-level, and if so what Dex module came from)
resolveExporterFun :: Exporter PName PName -> Check ExporterFun
resolveExporterFun ex =
  case ex of
    ExportTop f _ _ es _ ->
      case locThing f of
        Unqual x ->
          do
            rw <- getState
            let exTs = exporterTypes rw
            case Map.lookup x (localExporters rw) of
              Just t -> pure (f, \_ _ _ ty -> ExportLocal f { locThing = x } (Just ty), Forall [] [] [] t, [])
              Nothing ->
                case Map.lookup x (exportersByName exTs) of
                  Nothing -> reportError (UndefinedName f)
                  Just [(m,ty)] -> pure (f, \as bs cs t -> ExportTop f { locThing = QName { qModule = m, qName = x } } as bs cs (Just t), ty, es)
                  Just []  -> error "[BUG] `resolveExporterFun`"
                  Just ((m1,_) : (m2,_) : _) -> reportError (AmbiguousName f m1 m2)
        Qual q ->
          do
            rw <- getState
            let exTs = exporterTypes rw
            case Map.lookup (qName q) =<< Map.lookup (qModule q) (exportersByModule exTs) of
              Just ty -> pure (f, \as bs cs t -> ExportTop f { locThing = q } as bs cs (Just t), ty, es)
              Nothing -> reportError (UndefinedName f)
    ExportLocal {} -> error "[BUG] resolveExporterFun"
    

checkExporter ::
  Exporter PName PName -> BasicExporterType DDLTCon QName -> Check (Exporter DDLTCon QName, Subst QName)
checkExporter ex ty =
  do
    res <- resolveExporterFun ex
    checkExporter' Set.empty res ty

checkExporter' :: Set Name -> ExporterFun -> BasicExporterType DDLTCon QName -> Check (Exporter DDLTCon QName, Subst QName)
checkExporter' outSkol (eNm, mk, ty, args) et@(a :-> b) =
  do
    let ePs  = etExporterParams ty
        have = length args
        need = length ePs
    unless (have == need) (reportError (MalformedExporter eNm (Mismatch need have)))
    let arg :-> res = etType ty
    case matchMany arg a of
      Nothing -> reportError (DDLTypeMismatch eNm (Mismatch arg a))
      Just csu ->
        do
          xs <- mapM (const freshVar) (etForeignTypeVars ty)
          let mkV x     = TVar eNm { locThing = x }
              dparamTs  = map (apSubstT csu) (map mkV (etDDLTypeVars ty))
              fparamTs  = map mkV xs
              isu       = Map.fromList (zip (etForeignTypeVars ty) fparamTs)
              res'      = apSubstT isu res
          skolem <- Map.keysSet . tparamStatus <$> getState
          case unifyType (Set.fromList xs) (outSkol `Set.union` skolem) res' b of
            Nothing -> reportError (ForeignTypeMismatch eNm (Mismatch res' b))
            Just fsu ->
              do
                (newSu, newArgs) <- foldM checkExArg (fsu,[]) (zip ePs args)
                pure (mk dparamTs fparamTs (reverse newArgs) et, newSu)
              where
              checkExArg (fsu1, doneArgs) (p :-> q, ex_arg) =
                do 
                  let tgt = map (apSubstT csu) p :-> apSubstT (fsu1 @@ isu) q
                  (newArg,fsu2) <- checkExporter ex_arg tgt
                  pure (fsu2 @@ fsu1, newArg : doneArgs)
  

-- | Find a default top-level exporter to use.
findDefault :: SourceRange -> Type DDLTCon -> Check (QName, ExporterType DDLTCon QName )
findDefault rng t =
  do
    defs <- exporterDefaults . exporterTypes <$> getState
    let candidates = filter ((t `isMoreSpecificThan`) . fst) defs
    case best [] candidates of
      Just x -> pure x
      Nothing -> reportError (CannotFindDefault rng t)
  where
  x `isMoreSpecificThan` y = isJust (matchType y x)

  best before this =
    case this of
      (ty,x) : after
        | all (ty `isMoreSpecificThan`) before &&
          all (ty `isMoreSpecificThan`) (map fst after) -> Just x
        | otherwise -> best (ty : before) after
      [] -> Nothing



-- | Check a Daedalus expression.  Optionally rewrites locals from case
-- expressions to projections from union.
checkDDLExpr :: DDLExpr -> Check (DDLExpr, Type DDLTCon)
checkDDLExpr e =
  do
    let (x,sels) = splitDDLExpr e
    ty <- checkDDLVar x sels
    env <- getEnv
    let var n = x { locThing = n }
        e1 = 
           case Map.lookup (locThing x) =<< patternVars env of
             Nothing    -> e
             Just (y,u) -> foldl DDLSel (DDLVar (var y)) ((UnionSelector :. var u) : sels)
    pure (e1, ty)

--------------------------------------------------------------------------------
-- Daedalus variables
--------------------------------------------------------------------------------

-- | Introduce a variable that 
withCaseVar :: Loc Name -> Type DDLTCon -> (Name,Name) -> Check a -> Check a
withCaseVar x t sel = 
  withVar x t .
  updEnv \ro ->
    ro { patternVars = Map.insert (locThing x) sel <$> patternVars ro }


-- | Add a variable in scope for the duration of the given computation.
withVar :: Loc Name -> Type DDLTCon -> Check a -> Check a
withVar x t k =
  do
    statuses <- varTypes <$> getState
    let nm = locThing x
        oldStatus = Map.lookup nm statuses
      
    updState \rw -> rw { varTypes = Map.insert nm (locRange x, NotExported t) (varTypes rw) }
    a <- k
    newStatuses <- varTypes <$> getState
    case Map.lookup nm newStatuses of
      Nothing -> error "[BUG] `withVar` variable disappeared."
      Just (_,status) -> checkExported x [] status

    updState \rw -> rw {
      varTypes =
        case oldStatus of
          Nothing -> Map.delete nm (varTypes rw)
          Just yes -> Map.insert nm yes (varTypes rw)
    }
    pure a
      
-- | Check that the given name got fully exported.
checkExported :: Loc Name -> [Text] -> VarStatus -> Check ()
checkExported x ls status =
  case status of
    ExportedBy _ -> pure ()
    NotExported {} -> reportError (VarNotExported x (reverse ls))
    ExplodedBy _ _ fs -> mapM_ checkField (Map.toList fs)
      where
      checkField (f,stat) = checkExported x (f : ls) stat

-- | Validate a variable with some selectors.
-- We check that a variable is defined, and is not exported more than once.
checkDDLVar ::
  Loc Name     {-^ Variable being exported -} ->
  [Selector]   {-^ Selectors -} ->
  Check (Type DDLTCon)
checkDDLVar x ls0 =
  do statuses <- varTypes <$> getState
     let nm = locThing x
     case Map.lookup nm statuses of
        Nothing -> reportError (UndefinedName (Unqual <$> x))
        Just (rng, status) ->
          do
            (ty,newStatus) <- go x status ls0
            updState \rw -> rw { varTypes = Map.insert nm (rng, newStatus) (varTypes rw) }
            pure ty        
  where
  go curLoc curStatus todo =
    case curStatus of
      ExportedBy there -> reportError (AlreadyExported curLoc (locRange there))

      ExplodedBy ty there fields ->
        case todo of
          [] -> reportError (AlreadyExported curLoc (locRange there))
          (_ :. l) : more ->
            let lnm = nameToText (locThing l) in
            case Map.lookup lnm fields of
              Nothing -> reportError (InvalidSelector ty l)
              Just fieldStatus ->
                do
                  (rty,newStatus) <- go l fieldStatus more
                  pure (rty, ExplodedBy ty there (Map.insert lnm newStatus fields))

      NotExported ty ->
        case todo of
          [] -> pure (ty, ExportedBy curLoc)
          (_ :. l) : more ->
            case ty of
              Type tc targs szargs
                | TUser unm <- locThing tc ->
                do
                  tyDecls <- ddlTypesByTName . ddlTypes <$> getEnv
                  case Map.lookup unm tyDecls of
                    Nothing -> error "[BUG] `lookupVar` missing type definition"

                    Just tdecl ->
                      case Core.tDef tdecl of
                        Core.TStruct fields' ->
                          let lnm = nameToText (locThing l)
                              suV = Map.fromList (zip (Core.tTParamKValue tdecl) targs)
                              szV = Map.fromList (zip (Core.tTParamKNumber tdecl) szargs)
                              rng = locRange tc
                              fields = [ (f,coreTypeToType rng suV szV t) | (f,t) <- fields' ]
                          in
                          case lookup lnm fields of
                            Nothing -> reportError (InvalidSelector ty l)
                            Just fty ->
                              do
                                (rty, status) <- go l (NotExported fty) more
                                let f_status (f,t) =
                                      (f, if f == lnm then status
                                                    else NotExported t)
                                    newFields = Map.fromList (map f_status fields)
                                pure (rty, ExplodedBy fty curLoc newFields)
              
                        Core.TUnion {} -> reportError (InvalidSelector ty l)
                        Core.TBitdata {} -> error ("XXX: `lookupVar` bitdata")

              _ -> reportError (InvalidSelector ty l)

checkModule :: Module PName PName -> Check (Module DDLTCon QName)
checkModule mo =
    do
      updState \rw -> rw { curModule = moduleName mo }
      checkUnique (map ftName (moduleForeignTypes mo))
      checkUnique (map (fmap fromU . declName) (moduleDecls mo))
      mapM_ checkTypeAlias (moduleForeignTypes mo)
      mapM_ checkDeclSig (moduleDecls mo)
      ds1 <- mapM checkDecl (moduleDecls mo)
      pure mo { moduleDecls = ds1 }


runValidator :: DDLTypes -> Check a ->
  Either ValidationError a
runValidator ddlTys m = fst <$> runMonad m ro rw
  where
  ro = RO {
    ddlTypes = ddlTys,
    patternVars = mempty
  }

  rw = RW {
    curModule = nameFromText "<no module>",
    
    exporterTypes = ExporterTypes {
      exportersByName = mempty,
      exportersByModule = mempty,
      exporterDefaults = []
    },
    
    foreignTypesByName = mempty,
    foreignTypesByModule = mempty,

    tvarSeed = 0,
    tparamStatus = mempty,
    varTypes = mempty,
    localExporters = mempty
  }