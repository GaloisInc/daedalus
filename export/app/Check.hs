{-| Validation a specification and fill-in some details that can be
    auto computed. -}
module Check where

import Control.Monad
import Data.Map(Map)
import Data.Map qualified as Map
import AlexTools(SourceRange)

import Daedalus.Core qualified as Core
import Quote
import Name
import AST

data ValidationError = 
    UndefinedVariable LName
  | InvalidSelector Core.Type LName
  | AlreadyExported LName LName   -- ^ First exporter, repeated export
  | VarNotExported LName [Core.Label]
  | ShadowedVariable LName LName
  | TypeMismatch Core.Type Core.Type    -- XXX: Locations
  | SizeMismatch Core.SizeType Core.SizeType -- XXX: Location

data BasicExporterType = Core.Type :-> ForeignType
data ExporterType      = Forall {
  etDDLTypeVars     :: [Core.TParam],
  etForeignTypeVars :: [Name],
  etExporterParams  :: [BasicExporterType],
  etType            :: BasicExporterType
}


data RO = RO {
  exporterTypes :: Map Name ExportType,
  -- ^ Types of known exporters
 
  patternVars   :: Maybe (Map Name (LName, LName)),
  -- ^ If this is `Just` we desugar pattern variables to union selectors

  tyDefs :: Map Core.TName Core.TDecl 
  -- ^ Type definitions
}

newtype RW = RW {
  varTypes :: Map Name (SourceRange, VarStatus)
}

data VarStatus =
    NotExported Core.Type
  | ExportedBy LName
  | ExplodedBy Core.Type LName (Map Core.Label VarStatus)


checkDeclDef def =
  case def of
    DeclDef q -> undefined
    DeclCase x alts -> undefined



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

--------------------------------------------------------------------------------
-- Substitutions
--------------------------------------------------------------------------------

data Su = Su {
  suTypes :: Map Core.TParam Core.Type,
  suSizes :: Map Core.TParam Core.SizeType
}

emptySu :: Su
emptySu = Su { suTypes = mempty, suSizes = mempty }

bindType :: Core.TParam -> Core.Type -> Su -> M Su
bindType x ty su =
  case Map.lookup x (suTypes su) of
    Nothing -> pure su { suTypes = Map.insert x ty (suTypes su) }
    Just t
      | t == ty -> pure su
      | otherwise -> reportError (TypeMismatch t ty)
  
bindSize :: Core.TParam -> Core.SizeType -> Su -> M Su
bindSize x sz su =
  case Map.lookup x (suSizes su) of
    Nothing -> pure su { suSizes = Map.insert x sz (suSizes su) }
    Just s
      | s == sz -> pure su
      | otherwise -> reportError (SizeMismatch s sz)



--------------------------------------------------------------------------------
-- Matching types: we only bind variables in the first argument
-- Variables in the 2nd argument are treated as constants.
--------------------------------------------------------------------------------

matchType :: Core.Type -> Core.Type -> Su -> M Su
matchType pat ty su =
  case (pat,ty) of
    (Core.TStream,   Core.TStream)    -> pure su
    (Core.TInteger,  Core.TInteger)   -> pure su
    (Core.TBool,     Core.TBool)      -> pure su
    (Core.TFloat,    Core.TFloat)     -> pure su
    (Core.TDouble,   Core.TDouble)    -> pure su
    (Core.TUnit,     Core.TUnit)      -> pure su

    (Core.TParam a, _) -> bindType a ty su
    
    (Core.TUInt szP, Core.TUInt szT)  -> matchSizeType szP szT su
    (Core.TSInt szP, Core.TSInt szT)  -> matchSizeType szP szT su
    
    (Core.TArray a,  Core.TArray b)   -> matchType a b su
    (Core.TMaybe a,  Core.TMaybe b)   -> matchType a b su
    (Core.TBuilder a, Core.TBuilder b) -> matchType a b su
    (Core.TIterator a, Core.TIterator b) -> matchType a b su
    (Core.TUser ut1, Core.TUser ut2) -> matchUserType ut1 ut2 su
    (Core.TMap a b,  Core.TMap c d) -> matchMany matchType [a,b] [c,d] su
    _ -> reportError (TypeMismatch pat ty)

matchMany :: (a -> a -> Su -> M Su) -> [a] -> [a] -> Su -> M Su
matchMany f xs ys su =
  case (xs,ys) of
    ([],[]) -> pure su
    (a : as, b : bs) ->
      do
        su1 <- f a b su
        matchMany f as bs su1
    _ -> error "[BUG]: `matchMany` length mismatch"

matchSizeType :: Core.SizeType -> Core.SizeType -> Su -> M Su
matchSizeType pat sz su =
  case (pat,sz) of
    (Core.TSizeParam x, _) -> bindSize x sz su
    (Core.TSize a, Core.TSize b) | a == b -> pure su
    _ -> reportError (SizeMismatch pat sz)

matchUserType :: Core.UserType -> Core.UserType -> Su -> M Su
matchUserType u1 u2 su
  | Core.utName u1 == Core.utName u2 =
    do su1 <- matchMany matchType (Core.utTyArgs u1) (Core.utTyArgs u2) su
       matchMany matchSizeType (Core.utNumArgs u1) (Core.utNumArgs u2) su1
  | otherwise = reportError (TypeMismatch (Core.TUser u1) (Core.TUser u2))



--------------------------------------------------------------------------------
-- Validation Monad
--------------------------------------------------------------------------------

newtype M a = M (RO -> RW -> Either ValidationError (a,RW))

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
                        Core.TBitdata {} -> undefined

              _ -> reportError (InvalidSelector ty l)
