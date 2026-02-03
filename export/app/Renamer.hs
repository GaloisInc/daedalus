module Renamer (checkModule, RenamerError(..), Mismatch(..)) where

import Control.Monad
import Data.Set(Set)
import Data.Set qualified as Set
import Data.Map(Map)
import Data.Map qualified as Map

import AlexTools

import Daedalus.PP
import Daedalus.Core qualified as Core

import Monad
import Name
import Type
import AST

type Ren = M RO RW RenamerError

data RenamerError =
    UndefinedName (Loc PName)
  | AmbiguousName (Loc PName) Name Name  -- ^ modules containing conflicting definitions
  | MultipleDefinitions Name SourceRange SourceRange
  | MalformedType (Loc PName) (Mismatch Int) (Mismatch Int) -- value, size
  | InvalidTParam (Loc Name) (Mismatch TParamFlavor)
  | TParamShadowBuiltIn (Loc Name)
  | UnusedTParam (Loc Name)

data Mismatch a = Mismatch {
  expected :: a,
  actual   :: a
}

instance PP RenamerError where
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
    where
    mis m suf = if expected m == actual m
              then []
              else [ "Expected" <+> pp (expected m) <+> suf,
                    "Actual" <+> pp (actual m) <+> suf ]
    msg x f =
      let pre = text (prettySourcePosLong (sourceFrom (locRange x))) in
      case f (qu (locThing x)) of
        [] -> pre
        a : as -> (pre <+> a) $$ nest 2 (vcat as)
      
    qu a = backticks (pp a)



data RO = RO {
  curModule :: Name,
  foreignTypesByName :: Map Name [(Name,ForeignTypeDecl)],
  foreignTypesByModule :: Map Name (Map Name ForeignTypeDecl),
  ddlTypesByName :: Map Name [(Name,Core.TDecl)],
  ddlTypesByModule :: Map Name (Map Name Core.TDecl),
  exportersByName :: Map Name [Name],
  exportersByModule :: Map Name (Set Name),
  localExporters :: Set Name
}

newtype RW = RW {
  -- Local to a function
  tparamStatus :: Map Name (Maybe TParamFlavor)
}

-- | Indicates what kind of type parameter we have
data TParamFlavor = TParamDDL | TParamForeign
  deriving Eq

instance PP TParamFlavor where
  pp f =
    case f of
      TParamDDL -> "a Daedalus type"
      TParamForeign -> "a foreign type"

-- | Check if the given name is a type parameter of the given kind.
isTParam :: Loc Name -> TParamFlavor -> Ren Bool
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
                  


-- | Check a type alias
checkTypeAlias :: ForeignTypeDecl -> Ren ()
checkTypeAlias decl =
  do
    checkUnique (ftParams decl)
    mapM_ checkValidParam (ftDef decl)
    where
    ps = Set.fromList (map locThing (ftParams decl))
    checkValidParam x =
      unless (locThing x `Set.member` ps)
        (reportError (UndefinedName (Unqual <$> x)))
      
     
-- | Check that a bunch of parameters are distinct.
checkUnique :: [Loc Name] -> Ren ()
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
checkForeignType :: Type PName -> Ren (Type QName)
checkForeignType ty =
  case ty of
    Type nm ts [] ->
      do
        args <- mapM checkForeignType ts

        case locThing nm of

          Unqual x ->
            do
              let nm' = nm { locThing = x }
              isTP <- isTParam nm' TParamForeign
              if isTP
                then pure (TVar nm')
                else
                  do
                    env <- getEnv
                    case Map.lookup x (foreignTypesByName env) of
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
              env <- getEnv
              case Map.lookup n =<< Map.lookup m (foreignTypesByModule env) of
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
    "stream"  ~> (TStream,1,0),
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
checkDDLType :: Type PName -> Ren (Type DDLTCon)
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
                        case Map.lookup x (ddlTypesByName env) of
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
              case Map.lookup n =<< Map.lookup m (ddlTypesByModule env) of
                Nothing  -> reportError (UndefinedName nm)
                Just def -> mkTCUserApp def nm args szs

    _ -> error "[BUG] `checkForeignType`"

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
      pure (Type nm { locThing = con } args [])

checkExporterType ::
  BasicExporterType PName PName -> Ren (BasicExporterType DDLTCon QName)
checkExporterType (a :-> b) =
  do
    a' <- checkDDLType a
    b' <- checkForeignType b
    pure (a' :-> b')


checkDecl :: Decl PName PName -> Ren (Decl DDLTCon QName)
checkDecl d =
  do
    let tps   = declDDLTParams d
        tpMap = Map.fromList [ (locThing tp, tp) | tp <- tps ]
    checkUnique tps

    updState \rw -> rw { tparamStatus = const Nothing <$> tpMap }
    expT <- checkExporterType (declType d)
    rw <- getState
    let doneTPs   = tparamStatus rw
        unusedTPs = [ UnusedTParam (tpMap Map.! x) | (x,Nothing) <- Map.toList doneTPs ]
    case unusedTPs of
      x : _ -> reportError x
      [] -> pure ()

    expParams <- forM (declFunParams d) \(x,t) ->
                  do
                    t' <- checkExporterType t
                    pure (x,t')
    let addLocalExps ro =
          ro { localExporters = Set.fromList (map (locThing . fst) (declFunParams d)) }
    def <- updEnv addLocalExps (checkDeclDef (declDef d))
    env <- getEnv
    
    pure Decl {
      declDefault = declDefault d,
      declName    = QName (curModule env) . fromU <$> declName d,
      declDDLTParams = [ tp | tp <- tps, Map.lookup (locThing tp) doneTPs == Just (Just TParamDDL) ],
      declForeignTParams = [ tp | tp <- tps, Map.lookup (locThing tp) doneTPs == Just (Just TParamForeign) ],
      declFunParams = expParams,
      declArg = declArg d,
      declType = expT,
      declDef = def      
    }

fromU :: PName -> Name
fromU x =
  case x of
    Unqual y -> y
    Qual {} -> error "[BUG] `fromU` Qual"

    
checkDeclDef :: DeclDef PName PName -> Ren (DeclDef DDLTCon QName)
checkDeclDef def =
  case def of
    DeclDef code -> DeclDef <$> checkForeignCode code
    DeclCase x as -> DeclCase x <$> mapM checkAlt as
    DeclLoop loop -> DeclLoop <$> checkLoop loop
    DeclExtern -> pure DeclExtern
  where
  checkAlt (p,c) = (,) p <$> checkForeignCode c
  checkLoopTP x =
    do
      s <- getState
      case Map.lookup (locThing x) (tparamStatus s) of
        Nothing -> reportError (UndefinedName (Unqual <$> x))
        Just (Just TParamForeign) -> pure ()
        Just (Just f) -> reportError (InvalidTParam x (Mismatch TParamForeign f))
        Just Nothing -> error "[BUG] `checkDeclDef` checkTP Just Nothing"
  checkLoop l =
    do
      mapM_ checkLoopTP (loopInit l)
      let (a,b,c) = loopFor l
      c' <- checkForeignCode c
      pure l { loopFor = (a,b,c') }

checkForeignCode :: ForeignCode PName PName -> Ren (ForeignCode DDLTCon QName)
checkForeignCode code =
  case code of
    Direct e -> Direct <$> checkExportExpr e
    Splice q -> Splice <$> mapM checkExportExpr q

checkExportExpr :: ExportExpr PName PName -> Ren (ExportExpr DDLTCon QName)
checkExportExpr e =
  do
    f <- mapM checkExporter (exportWith e)
    pure ExportExpr { exportWith = f, exportExpr = exportExpr e, exportResult = Nothing }

checkExporter :: Exporter PName PName -> Ren (Exporter DDLTCon QName)
checkExporter (ExportTop f _ _ es) =
  do
    es' <- mapM checkExporter es
    let mk f' = pure (ExportTop f { locThing = f' } [] [] es')
    env <- getEnv
    case locThing f of
      Unqual g
        | g `Set.member` localExporters env -> pure (ExportLocal f { locThing = g })
        | otherwise ->
        case Map.lookup g (exportersByName env) of
          Nothing -> reportError (UndefinedName f)
          Just [m] -> mk (QName m g)
          Just [] -> error "[BUG] `checkExporter` []"
          Just (m1 : m2 : _) ->
            reportError (AmbiguousName f m1 m2)
      Qual q ->
        case Map.lookup (qModule q) (exportersByModule env) of
          Just defs | qName q `Set.member` defs -> mk q
          _ -> reportError (UndefinedName f)
checkExporter (ExportLocal {}) = error "[BUG] `checkExporter` local"

checkModule ::
  [(Name,Name,Core.TDecl)] ->
  [Module DDLTCon QName]         {- ^ Other modules -} -> 
  Module PName PName -> Either RenamerError (Module DDLTCon QName)
checkModule ddlTys otherMods mo = fst <$> runMonad check ro rw 
  where
  check =
    do
      checkUnique (map ftName (moduleForeignTypes mo))
      checkUnique (map (fmap fromU . declName) (moduleDecls mo))
      mapM_ checkTypeAlias (moduleForeignTypes mo)
      ds1 <- mapM checkDecl (moduleDecls mo)
      pure mo { moduleDecls = ds1 }

  getForeignTys m = [ (moduleName m, t) | t <- moduleForeignTypes m ]

  foreignTys =
    [ (m, locThing (ftName ty), ty)
    | tys <- getForeignTys mo : map getForeignTys otherMods,
      (m,ty) <- tys
    ]

  exps =
    [ (moduleName mo, fromU (locThing (declName d))) | d <- moduleDecls mo ] ++ 
    [ (moduleName m, qName (locThing (declName d)))
    | m <- otherMods, d <- moduleDecls m
    ]


  ro = RO {
    curModule = moduleName mo,

    ddlTypesByName =
      Map.fromListWith (++) [ (x, [(m,t)]) | (m,x,t) <- ddlTys ],
    ddlTypesByModule =
      Map.fromListWith Map.union [ (m, Map.singleton x t) | (m,x,t) <- ddlTys ],
    
    foreignTypesByName =
      Map.fromListWith (++) [ (x, [(m,t)]) | (m,x,t) <- foreignTys ],
    foreignTypesByModule =
      Map.fromListWith Map.union [ (m, Map.singleton x t) | (m,x,t) <- foreignTys ],

    exportersByName =
      Map.fromListWith (++) [ (x,[m]) | (m,x) <- exps ],
    exportersByModule =
      Map.fromListWith Set.union [ (m, Set.singleton x) | (m,x) <- exps ],

    localExporters = Set.empty
  }
  rw = RW {
    tparamStatus = Map.empty
  }