{-# Language ImplicitParams, ConstraintKinds, OverloadedStrings #-}
{-# Language BlockArguments #-}
{-# Language GeneralizedNewtypeDeriving #-}
-- | Validate types, for sanity.
module Daedalus.Core.TypeCheck where

import           Control.Monad         (forM, forM_, when, zipWithM_)
import           Data.Functor          (($>))
import           Data.List             (group, sort)
import           Data.Map              (Map)
import qualified Data.Map              as Map
import qualified MonadLib              as M

import           Daedalus.Core.Basics
import           Daedalus.Core.ByteSet
import           Daedalus.Core.Decl
import           Daedalus.Core.Expr
import           Daedalus.Core.Grammar
import           Daedalus.Core.Type
import           Daedalus.PP
import           Daedalus.Panic        (panic)
import           Daedalus.Rec          (forgetRecs)

checkModule :: Module -> Maybe TypeError
checkModule m =
  case runTCResult (checkModuleM m) of
    Left err -> Just err
    Right _  -> Nothing

checkModuleM :: Module -> TCResult ()
checkModuleM m =
  do let ?fenv = funEnv (mFFuns m)
     let ?benv = funEnv (mBFuns m)
     let ?genv = funEnv (mGFuns m)
     let ?tenv = Map.fromList [ (tName d, d) | d <- forgetRecs (mTypes m) ]
     mapM_ (checkFun checkExpr)    (mFFuns m)
     mapM_ (checkFun checkByteSet) (mBFuns m)
     mapM_ (checkFun checkGrammar) (mGFuns m)

funType :: Fun a -> (FName, ([Type], Type))
funType fu = (fName fu, (map typeOf (fParams fu), typeOf (fName fu)))

funEnv :: [Fun a] -> Map FName ([Type],Type)
funEnv = Map.fromList . map funType

checkFun :: (a -> Env -> TCResult Type) -> Fun a -> TCResult ()
checkFun check fun =
  case fDef fun of
    External -> pure ()
    Def e ->
      inContext ("function" <+> pp (fName fun))
      do let env = Map.fromList [ (x, typeOf x) | x <- fParams fun ]
         t <- check e env
         typeIs (typeOf (fName fun)) t


checkGrammar :: (GEnv,BEnv,FEnv,TEnv) => Grammar -> Env -> TCResult Type
checkGrammar gram env =
  case gram of
    Pure e -> inContext "argument of pure" $ checkExpr e env

    GetStream -> pure TStream

    SetStream e ->
      do t <- inContext "argument of SetStram" (checkExpr e env)
         inContext "SetStream" (typeIs TStream t)
         pure TUnit

    Match s m ->
      do t <- checkMatch m env
         pure case s of
                SemNo  -> TUnit
                SemYes -> t

    Fail _errS t mb ->
      do case mb of
           Nothing -> pure ()
           Just e  -> typeIs (TArray tByte) =<< checkExpr e env
         pure t

    Do_ g1 g2 ->
      do _ <- inContext "do head" $ checkGrammar g1 env
         inContext "do" $ checkGrammar g2 env

    Do x g1 g2 ->
      do t <- inContext "do head" $ checkGrammar g1 env
         inContext "do" $ checkGrammar g2 (extEnv x t env)

    Let x e g ->
      do t <- inContext "let expression" $ checkExpr e env
         inContext "let" $ checkGrammar g (extEnv x t env)

    OrBiased g1 g2 ->
      do t1 <- inContext "<| left"  $ checkGrammar g1 env
         t2 <- inContext "<| right" $ checkGrammar g2 env
         inContext "<|" $ isSameType t1 t2
         pure t1

    OrUnbiased g1 g2 ->
      do t1 <- inContext "| left"  $ checkGrammar g1 env
         t2 <- inContext "| right" $ checkGrammar g2 env
         inContext "|"             $ isSameType t1 t2
         pure t1

    Call f es ->
      do xs <- forM (zip [1..] es) \(n,e) ->
                  inContext (int n <+> "argument of" <+> pp f) $
                      checkExpr e env
         checkCall ?genv f xs

    Annot _a g -> checkGrammar g env

    GCase c -> checkCase checkGrammar c env

    Loop lc -> case lc of
      ManyLoop s _b l m_u g -> do
        inContext "Many lower" $ typeIs (tWord 64) =<< checkExpr l env
        case m_u of
          Nothing -> pure ()
          Just u -> inContext "Many upper" $ typeIs sizeType =<< checkExpr u env
        t <- inContext "Many body" $ checkGrammar g env
        case s of
          SemNo  -> pure TUnit
          SemYes -> pure (TArray t)
          
      RepeatLoop _b n e g -> do
        t <- inContext "many expr" $ checkExpr e env
        inContext "many body" $ checkGrammar g (extEnv n t env)
        
      MorphismLoop lm -> checkMorphism checkGrammar lm env
      
checkMatch :: (TEnv,BEnv,FEnv) => Match -> Env -> TCResult Type
checkMatch ma env =
  case ma of
    MatchByte bs ->

      do _ <- checkByteSet bs env
         pure tByte

    MatchBytes e ->
      do typeIs (TArray tByte) =<< checkExpr e env
         pure (TArray tByte)

    MatchEnd ->
      pure TUnit


checkByteSet :: (TEnv,BEnv,FEnv) => ByteSet -> Env -> TCResult Type
checkByteSet bs env =
  case bs of

    SetAny -> pure TBool

    SetSingle e ->
      do typeIs tByte =<< checkExpr e env
         pure TBool

    SetRange e1 e2 ->
      do typeIs tByte =<< checkExpr e1 env
         typeIs tByte =<< checkExpr e2 env
         pure TBool

    SetComplement b -> checkByteSet b env

    SetUnion b1 b2 -> checkByteSet b1 env >> checkByteSet b2 env

    SetIntersection b1 b2 -> checkByteSet b1 env >> checkByteSet b2 env

    SetLet x e b ->
      do t <- checkExpr e env
         checkByteSet b (extEnv x t env)

    SetCall f es ->
      do ts <- mapM (`checkExpr` env) es
         checkCall ?benv f ts

    SetCase c -> checkCase checkByteSet c env
    SetLoop lm -> checkMorphism checkByteSet lm env
      
checkVar :: Name -> Env -> TCResult Type
checkVar x env =
  let t1 = typeOf x
  in case Map.lookup x env of
       Just t | t == t1 -> pure t1
              | otherwise ->
                typeError "Type mistmatch:"
                          [ "Variable:" <+> backticks (pp x)
                          , "Expected:" <+> pp t
                          , "Actual:"   <+> pp t1
                          ]
       Nothing -> typeError "Undefined variable:"
                            [ "Variable:" <+> backticks (pp x) ]


checkExpr :: (FEnv,TEnv) => Expr -> Env -> TCResult Type
checkExpr expr env =
  case expr of
    Var x -> checkVar x env
    PureLet x e1 e2 ->
      do t <- checkExpr e1 env
         checkExpr e2 (extEnv x t env)

    Struct ut fields ->
      do def     <- getUserTypeM ut
         dFields <- case def of
                      TStruct fs -> pure fs
                      TUnion _   -> typeMismatch "struct" (TUser ut)
                      TBitdata _ bdef ->
                        case bdef of
                          BDStruct {} -> pure (getFields def)
                          BDUnion {}  -> typeMismatch "struct" (TUser ut)

         let checkField (l,e) =
               do t <- checkExpr e env
                  case lookup l dFields of
                    Nothing -> typeError "Malformed struct:"
                                  [ "Extra field:" <+> backticks (pp l) ]
                    Just t1
                      | t /= t1 -> typeError "Type mismtach:"
                                      [ "Field:" <+> backticks (pp l)
                                      , "Expected:" <+> pp t1
                                      , "Actual:" <+> pp t
                                      ]
                      | otherwise -> pure ()

         case [ l | (l,_) <- dFields, not (l `elem` map fst fields) ] of
           []    -> mapM_ checkField fields
           l : _ -> typeError "Missing field:" [ "Field:" <+> backticks (pp l) ]
         pure (TUser ut)

    ECase c -> checkCase checkExpr c env
    ELoop lm -> checkMorphism checkExpr lm env 
    Ap0 op0 ->
      inContext (pp op0) $
       checkOp0 op0

    Ap1 op1 e ->
      do t1 <- inContext ("argument of" <+> pp op1) $
               checkExpr e env
         inContext (pp op1) $ checkOp1 op1 t1


    Ap2 op2 e1 e2 ->
      do t1 <- inContext ("1st argument of" <+> pp op2) $
               checkExpr e1 env
         t2 <- inContext ("2nd argument of" <+> pp op2) $
               checkExpr e2 env
         inContext (pp op2) $ checkOp2 op2 t1 t2

    Ap3 op3 e1 e2 e3 ->
      do t1 <- checkExpr e1 env
         t2 <- checkExpr e2 env
         t3 <- checkExpr e3 env
         checkOp3 op3 t1 t2 t3

    ApN opN es -> checkOpN opN =<< mapM (`checkExpr` env) es


checkCase ::
  TEnv => (a -> Env -> TCResult Type) -> Case a -> Env -> TCResult Type
checkCase checkRHS (Case x alts) env =
  inContext "case"
  do t <- inContext ("discriminat" <+> pp x) $ checkVar x env
     inContext "exhaustive" (checkPatterns t (map fst alts))
     ts <- forM alts \(p,r) ->
           inContext ("alternative" <+> pp p)
              do ty <- checkRHS r env
                 pure (p,ty)
     case ts of
       [] -> typeError "Empty case" []
       (_,tr) : trs ->
         do forM_ trs \(p1,t1) ->
              inContext ("alternative" <+> pp p1)
                        (typeIs tr t1)
            pure tr

checkPatterns :: TEnv => Type -> [Pattern] -> TCResult ()
checkPatterns t ps
  | not (null repeated) = typeError "Repeated patterns in `case`"
                                    (map pp repeated)
  | num == 0            = typeError "Empty case" []
  | PAny <- last ps     =
    do user <- resolve
       mapM_ (checkPatType user) ps

  | otherwise =
    do user <- resolve
       case user of
         Just fs -> countNotExhaustive (length fs)
         Nothing ->
           case t of
             TBool           -> countNotExhaustive 2
             TMaybe {}       -> countNotExhaustive 2
             TInteger        -> notExhaustive
             TUInt (TSize n) -> countNotExhaustive (2^n)
             TSInt (TSize n) -> countNotExhaustive (2^n)
             TArray (TUInt (TSize 8)) -> notExhaustive
             _               -> notPat




  where
  repeated = [ x | (x : _ : _) <- group (sort ps) ]
  num      = length ps

  notPat = typeError "Type does not support pattern matching"
                        [ "Type:" <+> pp t ]

  notExhaustive = typeError "Missing branches in `case`" []
  countNotExhaustive n = when (n < num) notExhaustive

  resolve =
    case t of
      TUser ut ->
        do def <- getUserTypeM ut
           case def of
             TUnion fs  -> pure (Just (map fst fs))
             TStruct {} -> notPat
             TBitdata _ bdef ->
               case bdef of
                 BDUnion {}  -> pure (Just (map fst (getFields bdef)))
                 BDStruct {} -> notPat

      _        -> pure Nothing

  checkPatType ty p =
    case p of
      PBool {}    -> typeIs TBool t
      PNothing {} -> isMaybe t >> pure ()
      PJust {}    -> isMaybe t >> pure ()
      PNum {}     -> isIntegral t
      PBytes {}   -> isArray t >>= typeIs tByte
      PCon c
        | Just fs <- ty
        , c `elem` fs -> pure ()
        | otherwise   -> typeError "Malformed pattern"
                                     [ "Type:" <+> pp t
                                     , "Does not have construcotr:" <+> pp c
                                     ]

      PAny -> pure ()


checkOp0 :: Op0 -> TCResult Type
checkOp0 op =
  pure
  case op of
    Unit         -> TUnit
    IntL _ t     -> t
    FloatL _ t   -> t
    BoolL _      -> TBool
    ByteArrayL _ -> TArray (TUInt (TSize 8))
    NewBuilder t -> TBuilder t
    MapEmpty k v -> TMap k v
    ENothing t   -> TMaybe t

checkOp1 :: TEnv => Op1 -> Type -> TCResult Type
checkOp1 op arg =
  case op of
    CoerceTo t      -> isCoercible arg t        $> t
    IsEmptyStream   -> typeIs TStream arg       $> TBool
    Head            -> typeIs TStream arg       $> tByte
    StreamOffset    -> typeIs TStream arg       $> sizeType
    StreamLen       -> typeIs TStream arg       $> sizeType
    BytesOfStream   -> typeIs TStream arg       $> TArray tByte
    OneOf _         -> typeIs tByte   arg       $> sizeType
    Neg             -> isArith arg              $> arg

    BitNot          -> isWord arg               $> arg
    Not             -> typeIs TBool arg         $> TBool

    ArrayLen        -> isArray arg              $> sizeType

    WordToFloat     -> typeIs (tWord 32) arg    $> TFloat
    WordToDouble    -> typeIs (tWord 64) arg    $> TDouble

    IsNaN           -> isFP arg                 $> TBool
    IsInfinite      -> isFP arg                 $> TBool
    IsDenormalized  -> isFP arg                 $> TBool
    IsNegativeZero  -> isFP arg                 $> TBool

    Concat          -> TArray <$> (isArray =<< isArray arg)
    FinishBuilder   -> TArray <$> isBuilder arg

    NewIterator     -> supportsIterator arg     $> TIterator arg

    IteratorDone    -> isIterator arg           $> TBool

    IteratorKey     -> fst <$> (supportsIterator =<< isIterator arg)
    IteratorVal     -> snd <$> (supportsIterator =<< isIterator arg)
    IteratorNext    -> isIterator arg           $> arg

    EJust           -> pure (TMaybe arg)
    FromJust        -> isMaybe arg

    SelStruct t l   -> hasField l t arg         $> t
    InUnion ut l    -> hasCon l arg (TUser ut)  $> TUser ut

    FromUnion t l   -> hasCon l t arg           $> t


checkOp2 :: Op2 -> Type -> Type -> TCResult Type
checkOp2 op arg1 arg2 =
  case op of
    IsPrefix ->
      do typeIs (TArray tByte) arg1
         typeIs TStream        arg2
         pure TBool

    Drop ->
      do typeIs sizeType arg1
         typeIs TStream  arg2
         pure TStream

    Take ->
      do typeIs sizeType arg1
         typeIs TStream  arg2
         pure TStream

    Eq      -> isSameType arg1 arg2 $> TBool
    NotEq   -> isSameType arg1 arg2 $> TBool
    Leq     -> isSameType arg1 arg2 $> TBool
    Lt      -> isSameType arg1 arg2 $> TBool

    Add     -> isBinArith arg1 arg2
    Sub     -> isBinArith arg1 arg2
    Mul     -> isBinArith arg1 arg2
    Div     -> isBinArith arg1 arg2
    Mod     -> isBinArith arg1 arg2

    BitAnd  -> isBinArith arg1 arg2
    BitOr   -> isBinArith arg1 arg2
    BitXor  -> isBinArith arg1 arg2

    Cat ->
      do a <- isWord arg1
         b <- isWord arg2
         pure (tWord (a+b))

    LCat ->
      do isIntegral arg1
         _ <- isWord arg2
         pure arg1

    LShift ->
      do isIntegral arg1
         typeIs sizeType arg2
         pure arg1

    RShift ->
      do isIntegral arg1
         typeIs sizeType arg2
         pure arg1

    ArrayIndex ->
      do t <- isArray    arg1
         typeIs sizeType arg2
         pure t

    Emit ->
      do typeIs (TBuilder arg2) arg1
         pure arg1

    EmitArray ->
      do t <- isArray arg2
         typeIs (TBuilder t) arg1
         pure arg1

    EmitBuilder ->
      do t <- isBuilder arg2
         typeIs (TBuilder t) arg1
         pure arg1





    ArrayStream ->
      do typeIs (TArray tByte) arg1 -- name
         typeIs (TArray tByte) arg2 -- parse this
         pure TStream

    MapLookup ->
      do (k,v) <- isMap arg1
         typeIs k arg2
         pure (TMaybe v)

    MapMember ->
      do (k,_) <- isMap arg1
         typeIs k arg2
         pure TBool


checkOp3 :: Op3 -> Type -> Type -> Type -> TCResult Type
checkOp3 op arg1 arg2 arg3 =
  case op of
    RangeUp ->
      do isSameType arg1 arg2
         isSameType arg2 arg3
         isIntegral arg1
         pure (TArray arg1)

    RangeDown ->
      do isSameType arg1 arg2
         isSameType arg2 arg3
         isIntegral arg1
         pure (TArray arg1)

    MapInsert ->
      do (k,v) <- isMap arg1
         typeIs k arg2
         typeIs v arg3
         pure arg1

checkOpN :: FEnv => OpN -> [Type] -> TCResult Type
checkOpN op args =
  case op of

    ArrayL t ->
      do mapM_ (typeIs t) args
         pure (TArray t)

    CallF f -> checkCall ?fenv f args


checkCall :: Map FName ([Type], Type) -> FName -> [Type] -> TCResult Type
checkCall fenv f args =
  case Map.lookup f fenv of
    Just (as,r)
      | need == have ->
        do zipWithM_ isSameType as args
           pure r
      | otherwise ->
        typeError "Invlalid function call"
          [ "Expected:" <+> pp need <+> suff need
          , "Given:" <+> pp have <+> suff have
          ]
      where
      need   = length as
      have   = length args
      suff n = if n == 1 then "argument" else "arguments"
    Nothing -> typeError "Undefined function" [ "Name:" <+> pp f ]

checkMorphism :: (FEnv,TEnv) => (a -> Env -> TCResult Type) -> 
                 LoopMorphism a -> Env -> TCResult Type
checkMorphism chk lm env = case lm of
  FoldMorphism s e lc b -> do
    t_e <- inContext "for expr" $ checkExpr e env
    (env', _) <- lcEnv lc
    t_b <- inContext "for body" $ chk b (extEnv s t_e env')
    typeIs t_b t_e -- check result of body is the same as s
    pure t_b
    
  MapMorphism lc b -> do
    (env', colTy) <- lcEnv lc
    t_b <- inContext "for body" $ chk b env'
    pure $ case colTy of
      TArray _  -> TArray t_b
      TMap kt _ -> TMap kt t_b
      _ -> panic "Impossible" ["Collection type is invalid"]
      
  where
    lcEnv lc = do
      t <- inContext "loop collection" $ checkExpr (lcCol lc) env
      (kt, vt) <- supportsIterator t 
      let kenv = maybe env (\kn -> extEnv kn kt env) (lcKName lc)
      pure (extEnv (lcElName lc) vt kenv, t)

--------------------------------------------------------------------------------

extEnv :: Name -> Type -> Env -> Env
extEnv = Map.insert

type TEnv      = (?tenv :: Map TName TDecl)
type FEnv      = (?fenv :: Map FName ([Type],Type)) -- ^ values
type BEnv      = (?benv :: Map FName ([Type],Type)) -- ^ byte set
type GEnv      = (?genv :: Map FName ([Type],Type)) -- ^ grammar

newtype TCResult a  = TC (M.ReaderT [Doc] (M.ExceptionT TypeError M.Id) a)
  deriving (Functor,Applicative,Monad)

runTCResult :: TCResult a -> Either Doc a
runTCResult (TC m) = M.runId $ M.runExceptionT $ M.runReaderT [] m

type TypeError = Doc
type Env       = Map Name Type


getUserType :: TEnv => UserType -> Either Doc TDef
getUserType ut = runTCResult (getUserTypeM ut)

getUserTypeM :: TEnv => UserType -> TCResult TDef
getUserTypeM ut =
  do def <- case Map.lookup (utName ut) ?tenv of
              Nothing -> bad "Undefined user type:" ""
              Just d  -> pure (tDef d)
     case def of
       TStruct fs   -> TStruct <$> mapM instField fs
       TUnion  fs   -> TUnion  <$> mapM instField fs
       TBitdata {}  -> pure def -- no parameters to instantiate
  where
  instField (l,t) = do t1 <- instType t
                       pure (l,t1)
  instType t =
    case t of
      TParam (TP n) -> case drop n (utTyArgs ut) of
                         a : _ -> pure a
                         _ -> bad "Undefined type parmaeter:"
                                  ("Parameter:" <+> int n)
      TStream     -> pure t
      TUInt sz    -> TUInt <$> instSizeType sz
      TSInt sz    -> TSInt <$> instSizeType sz
      TInteger    -> pure t
      TFloat      -> pure t
      TDouble     -> pure t
      TBool       -> pure t
      TUnit       -> pure t
      TArray t1   -> TArray <$> instType t1
      TMaybe t1   -> TMaybe <$> instType t1
      TMap t1 t2  -> TMap <$> instType t1 <*> instType t2
      TBuilder t1 -> TBuilder <$> instType t1
      TIterator t1-> TIterator <$> instType t1
      TUser u     ->
        do ns <- mapM instSizeType (utNumArgs u)
           ts <- mapM instType     (utTyArgs u)
           pure (TUser u { utNumArgs = ns, utTyArgs = ts })

  instSizeType ty =
    case ty of
      TSize _ -> pure ty
      TSizeParam (TP n) ->
        case drop n (utNumArgs ut) of
          a : _ ->
            case a of
              TSize _ -> pure a
              _       -> bad "Uninstantiated numeric parameter"
                             ("Numeric parametr #:" <+> int n)
          _ -> bad "Undefined numeric parametr"
                             ("Numeric parametr #:" <+> int n)

  bad a b = typeError a [ "Type:" <+> pp (utName ut), b ]




typeError :: Doc -> [Doc] -> TCResult a
typeError x xs = TC
  do ctx <- M.ask
     let ys = xs ++ [ "Context", bullets (reverse ctx) ]
     M.raise (hang x 2 (bullets ys))

inContext :: Doc -> TCResult a -> TCResult a
inContext d (TC m) = TC (M.mapReader (d:) m)

typeMismatch :: Doc -> Type -> TCResult a
typeMismatch expected actual =
  typeError "Type mistmach"
              [ "Expected:" <+> expected
              , "Actual:"   <+> pp actual
              ]

typeIs :: Type -> Type -> TCResult ()
typeIs expected actual
  | expected == actual = pure ()
  | otherwise          = typeMismatch (pp expected) actual

isSameType :: Type -> Type -> TCResult ()
isSameType t1 t2
  | t1 == t2  = pure ()
  | otherwise = typeMismatch (pp t1) t2

isBinArith :: Type -> Type -> TCResult Type
isBinArith t1 t2 =
  do isSameType t1 t2
     isArith t1
     pure t1

isBinInt :: Type -> Type -> TCResult Type
isBinInt t1 t2 =
  do isSameType t1 t2
     isIntegral t1
     pure t1


hasField :: TEnv => Label -> Type -> Type -> TCResult ()
hasField l t ty =
  case ty of
    TUser ut ->
      do def <- getUserTypeM ut
         case def of
           TStruct fs -> check fs
           TUnion {} -> notStruct
           TBitdata _ bddef ->
             case bddef of
               BDStruct {} -> check (getFields bddef)
               BDUnion {}  -> notStruct


    _ -> typeMismatch "user defined type" ty
  where
  notStruct    = typeMismatch "struct" ty
  check fs = case lookup l fs of
              Nothing ->
                typeError "Malformed selector"
                   [ "Type:" <+> pp ty
                   , "Missing field:" <+> pp l
                   ]
              Just t1
                | t == t1   -> pure ()
                | otherwise ->
                  typeError "Malformed selector"
                    [ "Type:"     <+> pp ty
                    , "Field:"    <+> pp l
                    , "Declared:" <+> pp t1
                    , "Selector:" <+> pp t
                    ]

hasCon :: TEnv => Label -> Type -> Type -> TCResult ()
hasCon l t ty =
  case ty of
    TUser ut ->
      do def <- getUserTypeM ut
         case def of
           TStruct {} ->  notUnion
           TUnion fs  -> check fs
           TBitdata _ bddef ->
             case bddef of
               BDStruct {} -> notUnion
               BDUnion {}  -> check (getFields bddef)

    _ -> typeMismatch "user defined type" ty
  where
  notUnion = typeMismatch "union" ty
  check fs = case lookup l fs of
              Nothing ->
                typeError "Malformed constructor"
                   [ "Type:" <+> pp ty
                   , "Missing constructor:" <+> pp l
                   ]
              Just t1
                | t == t1   -> pure ()
                | otherwise ->
                  typeError "Malformed constructor"
                    [ "Type:"         <+> pp ty
                    , "Constructor argument of"  <+> pp l
                    , "Expected:"     <+> pp t1
                    , "Actual:"       <+> pp t
                    ]




-- XXX: TODO, unchecked for now
isCoercible :: Type -> Type -> TCResult ()
isCoercible _tFrom _tTo = pure ()

supportsIterator :: Type -> TCResult (Type,Type)
supportsIterator ty =
  case ty of
    TArray e  -> pure (sizeType, e)
    TMap k v  -> pure (k,v)
    _         -> typeMismatch "iterable type" ty

isArith :: Type -> TCResult ()
isArith ty =
  case ty of
    TUInt {}    -> pure ()
    TSInt {}    -> pure ()
    TInteger {} -> pure ()
    TFloat      -> pure ()
    TDouble     -> pure ()
    _           -> typeMismatch "arithmetic type" ty

isIntegral :: Type -> TCResult ()
isIntegral ty =
  case ty of
    TUInt {}    -> pure ()
    TSInt {}    -> pure ()
    TInteger {} -> pure ()
    _           -> typeMismatch "integral type" ty

isWord :: Type -> TCResult Integer
isWord ty =
  case ty of
    TUInt t ->
      case t of
        TSize n -> pure n
        p       -> typeError "Unexpected type parameter"
                              [ "Parameter:" <+> pp p ]
    _       -> typeMismatch "unsigned int" ty

isArray :: Type -> TCResult Type
isArray ty =
  case ty of
    TArray t  -> pure t
    _         -> typeMismatch "array" ty

isMap :: Type -> TCResult (Type,Type)
isMap ty =
  case ty of
    TMap k v -> pure (k,v)
    _        -> typeMismatch "map" ty

isBuilder :: Type -> TCResult Type
isBuilder ty =
  case ty of
    TBuilder t -> pure t
    _          -> typeMismatch "builder" ty

isFP :: Type -> TCResult ()
isFP ty =
  case ty of
    TFloat  -> pure ()
    TDouble -> pure ()
    _       -> typeMismatch "floating point type" ty

isIterator :: Type -> TCResult Type
isIterator ty =
  case ty of
    TIterator t -> pure t
    _           -> typeMismatch "iterator" ty

isMaybe :: Type -> TCResult Type
isMaybe ty =
  case ty of
    TMaybe t -> pure t
    _        -> typeMismatch "maybe" ty




