{-| Validation a specification and fill-in some details that can be
    auto computed. -}
module Check(checkModule) where

import Data.Maybe(isJust)
import Control.Monad
import Data.Text qualified as Text
import Data.Set(Set)
import Data.Set qualified as Set
import Data.Map(Map)
import Data.Map qualified as Map
import AlexTools(SourceRange,sourceFrom,prettySourcePosLong, prettySourcePos)

import Daedalus.PP
import Daedalus.Core qualified as Core

import Name
import AST
import Type
import Subst


checkModule :: Map Core.TName Core.TDecl -> Module -> Either ValidationError Module
checkModule tys m =
  runValidation tys
  do
    let ds = moduleDecls m
    env <- checkDeclSigs ds
    ds' <- withTopExporters env (mapM checkDecl ds)
    pure m { moduleDecls = ds' }

checkDeclSigs :: [Decl] -> M (Map Name (ExporterType,Bool))
checkDeclSigs ds =
  do
    xs <- mapM checkDeclSig ds
    let mp = Map.fromListWith (++) [ (nameName x,[(nameRange x,t)]) | (x,t) <- xs ]
        check x ys =
          case ys of
            [(_,t)] -> Right t
            (r,_) : (s,_) : _    -> Left (MultipleDefinitions x r s)
            [] -> error "[BUG] `checkDeclSigs` []"
        (bad,good) = Map.mapEitherWithKey check mp
    case Map.minView bad of
      Just (err, _) -> reportError err
      Nothing -> pure good

checkDeclSig :: Decl -> M (LName, (ExporterType,Bool))
checkDeclSig d =
  do
    let cps = declDDLTParams d
        cvs = freeTVarsCore (declArgType d)
        badC = [ l | (l,x) <- cps, not (x `Set.member` cvs) ]
    case badC of
      x : _ -> reportError (AmbiguousTParam x)
      [] -> pure ()
    let fps = declForeignTParams d
        fvs = freeTVarsForeignType (declResType d)
        badF = [ l | l <- fps, not (nameName l `Set.member` fvs) ]
    case badF of
      x : _ -> reportError (AmbiguousTParam x)
      _ -> pure ()
    let ty =
            Forall {
              etDDLTypeVars     = map snd cps,
              etForeignTypeVars = map nameName fps,
              etExporterParams  = map snd (declFunParams d),
              etType            = declArgType d :-> declResType d
            }
    pure (declName d, (ty, declDefault d))

checkDecl :: Decl -> M Decl
checkDecl d =
  case declDef d of
    DeclExtern -> pure d
    _ ->
      withTParams (Set.fromList (map snd (declDDLTParams d)))
                  (Set.fromList (map nameName (declForeignTParams  d))) $
      withExporters (declFunParams d) $
      withVar (declArg d) (declArgType d) $
      do
        def <- checkDeclDef (declDef d)
        pure d { declDef = def }

checkDeclDef :: DeclDef -> M DeclDef
checkDeclDef def =
  case def of
    DeclDef q       -> DeclDef <$> checkForeignCode q
    DeclCase x alts -> DeclCase x <$> checkCase x alts
    DeclExtern      -> pure DeclExtern
    DeclLoop l      -> DeclLoop <$> checkLoop l

checkForeignCode :: ForeignCode -> M ForeignCode
checkForeignCode code =
  case code of
    Splice q -> Splice <$> traverse checkExportExpr q
    Direct e -> Direct <$> checkExportExpr e

checkCase :: LName -> [(Pat, ForeignCode)] -> M [(Pat,ForeignCode)]
checkCase x alts =
  do
    ty <- lookupVar x []
    case ty of
      Core.TBool ->
        checkAlts x [("false", Core.TUnit), ("true", Core.TUnit)] alts

      Core.TMaybe f ->
        checkAlts x [("nothing", Core.TUnit), ("just", f)] alts

      Core.TUser ut ->
        do
          env <- getEnv
          case Map.lookup (Core.utName ut) (tyDefs env) of
            Nothing -> error "[BUG] `checkCase` Missing type"
            Just tdecl ->
              case Core.tDef tdecl of
                Core.TUnion opts -> checkAlts x opts alts
                Core.TBitdata {} -> error "XXX: `checkCase` `bitdata`"
                Core.TStruct {} -> reportError (InvalidCaseType x ty)

      _ -> reportError (InvalidCaseType x ty)

checkAlts ::
  LName -> [(Core.Label,Core.Type)] -> [(Pat, ForeignCode)] ->
  M [(Pat,ForeignCode)]
checkAlts disc needList = checkAll (Map.fromList needList) []
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
          checkAll (Map.delete (Name.toText (nameName nm)) mp) (p':done) more


  check mp (pat@(PCon nm mbX),code) =
    let lab = Name.toText (nameName nm)
    in
    case Map.lookup lab mp of
      Nothing -> reportError (InvalidCon nm)
      Just ty ->
        case (ty, mbX) of
          (Core.TUnit, Nothing) ->
            do
              rhs <- checkForeignCode code
              pure (pat, rhs)
          (_, Nothing) -> reportError (VarNotExported disc [lab])
          (ft, Just f) ->
            withCaseVar f ft (disc,nm)
            do
              code' <- checkForeignCode code
              pure (pat, code')
            
checkLoop :: Loop -> M Loop
checkLoop l =
  do
    mapM_ checkParam (loopInit l)
    let (vs,xs,body) = loopFor l
    ty <- lookupVar xs []
    body' <-
      case ty of
        Core.TArray a -> checkBody xs vs [a] body
        Core.TMap k v -> checkBody xs vs [k,v] body
        Core.TBuilder a -> checkBody xs vs [a] body
        _ -> reportError (InvalidForType xs ty)
    pure l { loopFor = (vs,xs,body') }
  where
  checkParam x =
    do
      env <- getEnv
      unless (nameName x `Set.member` foreignTypeParams env)
        (reportError (UndefinedVariable x))
  checkBody xs vs ts body
    | have /= need = reportError (InvalidForBinders xs need have)
    | Just ((a,(r1,r2)),_) <- repeated =
      reportError (MultipleDefinitions a r1 r2)
    | otherwise =
      foldr (uncurry withVar) (checkForeignCode body) (zip vs ts)
        where
        have = length vs
        need = length ts
        repeated = Map.minViewWithKey
                 $ Map.mapMaybe mult
                 $ Map.fromListWith (++) [ (nameName x,[nameRange x]) | x <- vs ]
        mult m =
          case m of
            a : b : _ -> Just (a,b)
            _ -> Nothing


checkExportExpr :: ExportExpr -> M ExportExpr
checkExportExpr ex =
  do
    (arg, t) <- checkDDLExpr (exportExpr ex)
    r <- freshVar
    let rt = ForeignTVar LName { nameRange = exportExprRange ex, nameName = r }
    e <- 
      case exportWith ex of
        Nothing ->
          do
            let rng = exportExprRange ex
            x <- findDefault rng t
            pure (ExportTop LName { nameName = x, nameRange = rng } [] [])
        Just e  -> pure e  
    (e', su) <- checkExporter e (t :-> rt)
    let e'' = apForeignSubst su e'
        resT = apForeignSubst su rt
    pure ExportExpr {
      exportWith = Just (apForeignSubst su e''),
      exportExpr = arg,
      exportResult = Just resT
    }

findDefault :: SourceRange -> Core.Type -> M Name
findDefault rng t =
  do
    env <- getEnv
    let mp = Map.mapMaybe matchesThis (exporterTypes env)
    case best [] (Map.toList mp) of
      Just x -> pure x
      Nothing -> reportError (CannotFindDefault rng t)
  where
  matchesThis (et,d)
    | d =
      case etType et of
        a :-> _ -> matchType t a emptySu >> pure a
    | otherwise = Nothing

  x `isMoreSpecificThan` y = isJust (matchType y x emptySu)
  best before this =
    case this of
      (x,ty) : after
        | all (ty `isMoreSpecificThan`) before &&
          all (ty `isMoreSpecificThan`) (map snd after) -> Just x
        | otherwise -> best (ty : before) after
      [] -> Nothing
  
-- | Check a Daedalus expression.
checkDDLExpr :: DDLExpr -> M (DDLExpr, Core.Type)
checkDDLExpr e@(DDLExpr x sels) =
  do
    ty <- lookupVar x sels
    env <- getEnv
    let e1 = 
           case Map.lookup (nameName x) =<< patternVars env of
             Nothing    -> e
             Just (y,u) -> DDLExpr y ((UnionSelector :. u) : sels)
    pure (e1, ty)


checkExporter :: Exporter -> BasicExporterType -> M (Exporter, ForeignSubst)
checkExporter e ty =
  case split e [] of
    (f,es) -> checkExporter' f es ty
  where
  split ex as =
    case ex of
      ExportTop f _ _ -> (f, as)
      ExportApp f arg -> split f (arg : as)

-- | Check that the given exporter has the provided type.
checkExporter' :: LName -> [Exporter] -> BasicExporterType -> M (Exporter, ForeignSubst)
checkExporter' f args (a :-> b) =
  do
    env <- getEnv
    case Map.lookup (nameName f) (exporterTypes env) of
      Nothing -> reportError (UndefinedVariable f)
      Just (ty,_) ->
        do
          let have = length args
              need = length (etExporterParams ty)
          unless (have == need) (reportError (ExportArgMismatch f need have))
          let arg :-> res = etType ty
          case matchType arg a emptySu of
            Nothing -> reportError (TypeMismatch (nameRange f) arg a)
            Just csu ->
              do
                xs <- mapM (const freshVar) (etForeignTypeVars ty)
                let mk x = ForeignTVar f { nameName = x }
                    isu  = Map.fromList (zip (etForeignTypeVars ty) (map mk xs))
                    res' = apForeignSubst isu res
                case unifyForeignType (Set.fromList xs) (foreignTypeParams env) res' b of
                  Nothing -> reportError (ForeignTypeMismatch (nameRange f) res' b)
                  Just fsu ->
                    do
                      let cts = [ suTypes csu Map.! x | x <- etDDLTypeVars ty ]
                      foldM checkExArg (ExportTop f cts (map mk xs),fsu)
                                              (zip (etExporterParams ty) args)
                    where
                    checkExArg (e,fsu1) (p :-> q, ex_arg) =
                      do 
                        let tgt = apSubstCore csu p :-> apForeignSubst (fsu1 @@ isu) q
                        (newArg,fsu2) <- checkExporter ex_arg tgt
                        pure (ExportApp e newArg, fsu2 @@ fsu1)
                      
                    
    




data ValidationError = 
    UndefinedVariable LName
  | InvalidSelector Core.Type LName
  | AlreadyExported LName LName   -- ^ First exporter, repeated export
  | VarNotExported LName [Core.Label]
  | ShadowedVariable LName LName
  | TypeMismatch SourceRange Core.Type Core.Type
  | ForeignTypeMismatch SourceRange ForeignType ForeignType
  | ExportArgMismatch LName Int Int -- ^ Need, have
  | InvalidForType LName Core.Type
  | InvalidForBinders LName Int Int -- ^ Need, have
  | InvalidCaseType LName Core.Type
  | InvalidCon LName
  | MissingCase LName Core.Label
  | MultipleDefinitions Name SourceRange SourceRange
  | AmbiguousTParam LName
  | CannotFindDefault SourceRange Core.Type

instance PP ValidationError where
  pp err =
    case err of

      UndefinedVariable x ->
        ppErr (sourceFrom (nameRange x))
          ("Undefined variable" <+> backticks (pp (nameName x)))

      InvalidSelector t l ->
        ppErr (sourceFrom (nameRange l))
          ("Type" <+> backticks (pp t) <+>
            "does not have selector" <+> backticks (pp l))

      AlreadyExported x y ->
        ppErr (sourceFrom (nameRange y)) $
          vcat [ "Variable" <+> backticks (pp (nameName x)) <+> "was already exported."
               , nest 2 ("Other export:" <+> ppLoc (sourceFrom (nameRange x)))
               ]
      VarNotExported x ls ->
        ppErr (sourceFrom (nameRange x))
          (what <+> thing <+> "was never exported.")
        where what = if null ls then "Variable" else "Field"
              thing = foldl (\doc l -> doc <.> "." <.> pp l) (pp (nameName x)) ls

      ShadowedVariable x y ->
        ppErr (sourceFrom (nameRange y)) $
          vcat [ "Variable was already defined.",
                 nest 2 ("Previous definition:" <+> ppLoc (sourceFrom (nameRange x))) ]

      TypeMismatch rng t1 t2 ->
        ppErr (sourceFrom rng) $
          vcat [ "Daedalus type mismatch:"
               , nest 2 $ "Expected:" <+> pp t1
               , nest 2 $ "Actual:" <+> pp t2
               ]

      ForeignTypeMismatch rng t1 t2 ->
        ppErr (sourceFrom rng) $
          vcat [ "Foreign type mismatch:"
               , nest 2 $ "Expected:" <+> pp t1
               , nest 2 $ "Actual:" <+> pp t2
               ]

      ExportArgMismatch f need have
        | need < have ->
          ppErr (sourceFrom (nameRange f))
            ("Export function" <+> backticks (pp (nameName f)) <+> "needs" <+>
              pp (have - need) <+> "more exporter arguments.")
        | otherwise ->
          ppErr (sourceFrom (nameRange f))
            ("Export function" <+> backticks (pp (nameName f)) <+>
              "has been applied to" <+> pp (need - have) <+> "too many exporter arguments.")

      InvalidForType x t ->
        ppErr (sourceFrom (nameRange x)) $
          vcat [ "Variable" <+> backticks (pp x) <+> "does not support iteration."
               , nest 2 ("It has type:" <+> pp t)
          ]

      InvalidForBinders x need have ->
        ppErr (sourceFrom (nameRange x))
          ("Iterating over" <+> backticks (pp x) <+> "requires" <+>
            pp need <+> "variables, but we have" <+> pp have <.> ".")

      InvalidCaseType x t ->
        ppErr (sourceFrom (nameRange x)) $
          vcat [ "Cannot case on" <+> backticks (pp x) <.> "."
               , nest 2 ("It has type:" <+> pp t) ]

      InvalidCon l ->
        ppErr (sourceFrom (nameRange l))
          ("Invalid case alternative:" <+> backticks (pp (nameName l)) <.> ".")

      MissingCase x l ->
        ppErr (sourceFrom (nameRange x))
          ("Missing case alternative:" <+> backticks (pp l))

      MultipleDefinitions x r1 r2 ->
        ppErr (sourceFrom r2) $
          vcat [ "Multiple definitions for" <+> backticks (pp x)
               , nest 2 ("Other definition:" <+> ppLoc (sourceFrom r1)) ]
      AmbiguousTParam x ->
        ppErr (sourceFrom (nameRange x)) $
          vcat  [ "Type parameter" <+> backticks (pp (nameName x)) <+> "is ambigous."
                , nest 2 "It needs to appear in input or output of the exporter."
               ]
      CannotFindDefault rng ty ->
        ppErr (sourceFrom rng)
          ("Cannot determine a default exporter for type" <+> backticks (pp ty))
    where
    ppLoc x  = text (prettySourcePos x)
    ppErr l msg = text (prettySourcePosLong l) <.> ":" <+> msg

data VarStatus =
    NotExported Core.Type
  | ExportedBy LName
  | ExplodedBy Core.Type LName (Map Core.Label VarStatus)


            



--------------------------------------------------------------------------------
-- Validation Monad
--------------------------------------------------------------------------------

newtype M a = M (RO -> RW -> Either ValidationError (a,RW))

runValidation :: Map Core.TName Core.TDecl -> M a -> Either ValidationError a
runValidation defs (M m) = fmap fst (m ro rw)
  where
  ro = RO {
    exporterTypes = mempty,
    tyDefs = defs,
    patternVars = Just mempty,
    ddlTyParams = mempty,
    foreignTypeParams = mempty
  }
  rw = RW {
    varTypes = mempty,
    tvarSeed = 0
  }

data RO = RO {
  exporterTypes :: Map Name (ExporterType,Bool),
  -- ^ Types of known exporters.  `Bool` indicates if this is a default.

  tyDefs :: Map Core.TName Core.TDecl,
  -- ^ Daedalus type definitions

  -- Local to one exporter

  patternVars   :: Maybe (Map Name (LName, LName)),
  -- ^ If this is `Just` we desugar pattern variables to union selectors

  ddlTyParams :: Set Core.TParam,
  -- ^ Daedalus type variables that are in scope

  foreignTypeParams :: Set Name
  -- ^ Foreign type variables that are in scope
  
}

data RW = RW {
  varTypes  :: Map Name (SourceRange, VarStatus),
  -- ^ Type of Daedalus of variables, together with their export status
  -- for linearity validation.

  tvarSeed :: !Int
  -- ^ Used to generate fresh foreign unification variables.
}


instance Functor M where
  fmap = liftM

instance Applicative M where
    pure a = M (\_ rw -> Right (a,rw))
    (<*>)   = ap

instance Monad M where
  M m >>= f = M \ro rw ->
    case m ro rw of
      Left err -> Left err
      Right (a,rw1)  ->
        do let M m1 = f a
           m1 ro rw1

freshVar :: M Name
freshVar = M \_ rw ->
  let n = tvarSeed rw
      nm = Name.fromText ("fresh-" <> Text.pack (show n))
  in pure (nm, rw { tvarSeed = n + 1 })

getEnv :: M RO
getEnv = M \ro rw -> Right (ro,rw)

getVarTypes :: M (Map Name (SourceRange, VarStatus))
getVarTypes = M \_ rw -> Right (varTypes rw, rw)

updVarTypes ::
  (Map Name (SourceRange, VarStatus) -> Map Name (SourceRange, VarStatus)) -> M ()
updVarTypes f = M \_ rw -> Right ((), rw { varTypes = f (varTypes rw) })

-- | Abort further checking and report the given error.
reportError :: ValidationError -> M a
reportError e = M \_ _ -> Left e

withTParams :: Set Core.TParam -> Set Name -> M a -> M a
withTParams xs ys (M m) = M \ro rw ->
  m ro { ddlTyParams = xs, foreignTypeParams = ys } rw

withTopExporters :: Map Name (ExporterType,Bool) -> M a -> M a
withTopExporters mp (M m) = M \ro rw ->
  m ro { exporterTypes = mp `Map.union` exporterTypes ro } rw

withExporters :: [(LName,BasicExporterType)] -> M a -> M a
withExporters xs (M m) = M \ro rw ->
  m ro { exporterTypes = Map.union new (exporterTypes ro) } rw
  where
  new = Map.fromList [ (nameName x, (Forall [] [] [] t, False)) | (x,t) <- xs ]

withCaseVar :: LName -> Core.Type -> (LName,LName) -> M a -> M a
withCaseVar f t b (M k) =
  withVar f t $ M \ro rw ->
    k ro { patternVars = (Map.insert (nameName f) b) <$> patternVars ro } rw

-- | Add a variable in scope for the duration of the given computation.
withVar :: LName -> Core.Type -> M a -> M a
withVar x t k =
  do
    statuses <- getVarTypes
    let nm = nameName x
    case Map.lookup nm statuses of
      Nothing ->
        do
          updVarTypes (Map.insert nm (nameRange x, NotExported t))
          a <- k
          newStatuses <- getVarTypes
          case Map.lookup nm newStatuses of
            Nothing -> error "[BUG] `withVar` variable disappeared."
            Just (_,status) -> checkExported x [] status
          updVarTypes (Map.delete nm)
          pure a
      Just (rng,_) -> reportError (ShadowedVariable x { nameRange = rng } x)

-- | Check that the given name got fully exported.
checkExported :: LName -> [Core.Label] -> VarStatus -> M ()
checkExported x ls status =
  case status of
    ExportedBy _ -> pure ()
    NotExported {} -> reportError (VarNotExported x (reverse ls))
    ExplodedBy _ _ fs -> mapM_ checkField (Map.toList fs)
      where
      checkField (f,stat) = checkExported x (f : ls) stat

-- | Validate a variable with some selectors.
-- We check that a variable is defined, and is not exported more than once.
lookupVar ::
  LName                     -> {-^ Variable being exported -}
  [Selector]                -> {-^ Selectors -}
  M Core.Type
lookupVar x ls0 =
  do statuses <- getVarTypes
     let nm = nameName x
     case Map.lookup nm statuses of
        Nothing -> reportError (UndefinedVariable x)
        Just (rng, status) ->
          do
            (ty,newStatus) <- go x status ls0
            updVarTypes (Map.insert nm (rng, newStatus))
            pure ty        
  where
  go curLoc curStatus todo =
    case curStatus of
      ExportedBy there -> reportError (AlreadyExported there curLoc)

      ExplodedBy ty there fields ->
        case todo of
          [] -> reportError (AlreadyExported there curLoc)
          (_ :. l) : more ->
            let lnm = Name.toText (nameName l) in
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
              Core.TUser ut ->
                do
                  tyDecls <- tyDefs <$> getEnv
                  case Map.lookup (Core.utName ut) tyDecls of
                    Nothing -> error "[BUG] `lookupVar` missing type definition"

                    Just tdecl ->
                      case Core.tDef tdecl of
                        Core.TStruct fields ->
                          let lnm = Name.toText (nameName l) in
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