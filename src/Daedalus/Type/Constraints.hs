{-# Language OverloadedStrings, BlockArguments, FlexibleInstances #-}
module Daedalus.Type.Constraints (simplifyConstraints, unify) where

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe(fromMaybe)
import Control.Monad(when)

import Daedalus.PP
import Daedalus.SourceRange

import Daedalus.AST
import Daedalus.Type.AST
import Daedalus.Type.Monad
import Daedalus.Type.Subst


data CtrStatus = Solved | Unsolved
  deriving Show

isSameCtr :: STCMonad m => Located Constraint -> Located Constraint -> m Bool
isSameCtr cNew cOld =
  case (thingValue cNew, thingValue cOld) of
    (Numeric x, Numeric y) | x == y -> pure True

    (HasStruct x1 l1 t1, HasStruct x2 l2 t2)
      | x1 == x2 && l1 == l2 ->
        do unify (cNew, t1) (cOld,t2)
           pure True

    (HasUnion x1 l1 t1, HasUnion x2 l2 t2)
      | x1 == x2 && l1 == l2 ->
        do unify (range cNew, t1) (cOld,t2)
           pure True

    (Coerce l1 t1 t2, Coerce l2 s1 s2) ->
      pure (l1 == l2 && t1 == s1 && t2 == s2)

    (Literal i t1, Literal j t2) ->
      pure (i == j && t1 == t2)

    (ColElType c e1, ColElType c' e2)
      | c == c' -> unify (cNew,e1) (cOld,e2) *> pure True

    (ColKeyType c k1, ColKeyType c' k2)
      | c == c' -> unify (cNew,k1) (cOld,k2) *> pure True

    (Traversable t1, Traversable t2) -> pure (t1 == t2)

    (Mappable t1 s1, Mappable t2 s2) -> pure (t1 == t2 && s1 == s2)

    -- XXX: CAdd

    -- no case for IsNamed, TyDef because we don't generalize those.

    _ -> pure False


isMappable :: (STCMonad m, HasRange r) => r -> Type -> Type -> m CtrStatus
isMappable r t s =
  case t of
    TVar {} ->
      case s of
        TVar {}        -> pure Unsolved
        Type TArray {} -> mkArray t
        Type TMap {}   -> mkMap t
        _              -> bad

    Type TArray {}  -> mkArray s
    Type TMap {}    -> mkArray s
    _               -> bad

  where
  bad = reportDetailedError r "Type does not support `map`"
          [ "Input type:" <+> pp t
          , "Output type:" <+> pp s
          ]

  mkArray x = do el <- newTVar r KValue
                 unify (tArray el) (r,x)
                 pure Solved

  mkMap x   = do k  <- newTVar r KValue
                 el <- newTVar r KValue
                 unify (tMap k el) (r,x)
                 pure Solved


isTraversable :: (STCMonad m, HasRange r) => r -> Type -> m CtrStatus
isTraversable r t =
  case t of
    TVar{}  -> pure Unsolved

    Type TArray {} -> pure Solved
    Type TMap {}   -> pure Solved
    -- Signed?

    _ -> reportDetailedError r "Cannot iterate over this type" [ pp t ]


isColElType :: (STCMonad m, HasRange r) => r -> Type -> Type -> m CtrStatus
isColElType r t e =
  case t of
    TVar {}         -> pure Unsolved

    Type (TArray a) -> unify a (r,e) *> pure Solved
    Type (TMap _ a) -> unify a (r,e) *> pure Solved
    Type TInteger   -> unify t (r,e) *> pure Solved
    Type TUInt{}    -> unify t (r,e) *> pure Solved

    _ -> reportDetailedError r "Invalid collection type"
              [ "Collection:" <+> pp t
              , "Element type:" <+> pp e
              ]



isColKeyType :: (STCMonad m, HasRange r) => r -> Type -> Type -> m CtrStatus
isColKeyType r t e =
  case t of
    TVar {}         -> pure Unsolved

    Type TArray {}  -> unify tInteger (r,e) *> pure Solved
    Type (TMap a _) -> unify a        (r,e) *> pure Solved
    Type TInteger   -> unify t        (r,e) *> pure Solved
    Type TUInt{}    -> unify t        (r,e) *> pure Solved

    _ -> reportDetailedError r "Invalid collection type"
              [ "Collection:" <+> pp t
              , "Key type:" <+> pp e
              ]



{-
isTraversable ::
  (STCMonad m, HasRange r) => r -> Type -> Type -> Type -> m CtrStatus
isTraversable r t k v =
  case t of
    TVar _  -> pure Unsolved

    Type (TArray el) ->
      do unify tInteger (r,k)   -- XXX: generalize?
         unify el (r,v)
         pure Solved

    Type (TMap tk tv) ->
      do unify tk (r,k)
         unify tv (r,v)
         pure Solved

    Type TInteger  -> numTrav
    Type (TUInt _) -> numTrav
    -- Signed?

    _ -> reportDetailedError r "Cannot iterate over this type" [ pp t ]

  where
  numTrav = do unify t (r,k)
               unify t (r,v)
               pure Solved
-}


isAdd :: (STCMonad m,HasRange r) => r -> Type -> Type -> Type -> m CtrStatus
isAdd r t1 t2 t3 =
  case (t1,t2,t3) of
    (Type (TNum x), Type (TNum y), Type (TNum z))
       | x + y == z -> pure Solved
       | otherwise  -> reportDetailedError r "Width mismatch"
                           [ "Expected:" <+> pp z
                           , "Actual:" <+> pp (x + y)
                           ]

    (Type (TNum 0), _, _) -> unify t2 (r,t3) >> pure Solved
    (_, Type (TNum 0), _) -> unify t1 (r,t3) >> pure Solved
    (_, _, Type (TNum 0)) -> do unify (tNum 0) (r, t1)
                                unify (tNum 0) (r, t2)
                                pure Solved

    (Type (TNum x), Type (TNum y), _) ->
      unify (tNum (x + y)) (r,t3) >> pure Solved

    (Type (TNum x), _, Type (TNum z))
      | res >= 0    -> unify (tNum res) (r,t2) >> pure Solved
      | otherwise -> reportDetailedError r "Insufficient width"
                          [ "Available width:" <+> pp z
                          , "Need at least:" <+> pp x
                          ]
      where res = z - x

    (_, Type (TNum y), Type (TNum z))
      | res >= 0  -> unify (tNum res) (r,t1) >> pure Solved
      | otherwise -> reportDetailedError r "Insufficient width"
                          [ "Available width:" <+> pp z
                          , "Need at least:"   <+> pp y
                          ]
      where res = z - y

    _ -> pure Unsolved


isNumeric :: (STCMonad m, HasRange r) => r -> Type -> m CtrStatus
isNumeric r ty =
  case ty of
    Type TInteger   -> pure Solved
    Type (TUInt _)  -> pure Solved
    Type (TSInt _)  -> pure Solved
    TVar _          -> pure Unsolved
    _               -> reportDetailedError r "Not a numeric type."
                          [ "Type:" <+> pp ty ]


hasStruct :: (STCMonad m, HasRange r) =>
            r -> Type -> Label -> Type -> m CtrStatus
hasStruct r ty l fty =
  case ty of
    TVar _ -> pure Unsolved

    TCon c ts ->
      do mb <- lookupTypeDef c
         case mb of
           Nothing -> pure Unsolved
           Just td -> case tctyDef td of
                        TCTyStruct fs | Just fty1 <- lookup l fs ->
                          do let su = Map.fromList (zip (tctyParams td) ts)
                             unify (apSubstT su fty1) (r,fty)
                             pure Solved
                        _ -> doErr
    _ -> doErr
  where
  doErr = reportDetailedError r
            ("Type does not have field" <+> backticks (pp l))
            [ "Problem type:" <+> pp ty ]




hasUnion :: (STCMonad m, HasRange r) =>
            r -> Type -> Label -> Type -> m CtrStatus
hasUnion r ty l fty =
  case ty of
    TVar _ -> pure Unsolved

    TCon c ts ->
      do mb <- lookupTypeDef c
         case mb of
           Nothing -> pure Unsolved
           Just td -> case tctyDef td of
                        TCTyUnion fs | Just fty1 <- lookup l fs ->
                          do let su = Map.fromList (zip (tctyParams td) ts)
                             unify (apSubstT su fty1) (r,fty)
                             pure Solved
                        _ -> doErr

    _ -> doErr
  where
  doErr = reportDetailedError r
            ("Type does not have tag" <+> backticks (pp l))
            [ "Problem type:" <+> pp ty ]


-- XXX: lossy
isCoercible :: (STCMonad m, HasRange r) =>
               r -> Lossy -> Type -> Type -> m CtrStatus
isCoercible r lossy tt1 tt2 =
  case tt1 of
    TVar _   -> pure Unsolved
    TCon {} -> nope
    Type t1 ->
      case t1 of
        TInteger  -> fromInt
        TUInt x   -> fromUInt x
        TSInt x   -> fromSInt x
        _         -> refl
  where
  nope = reportDetailedError r "Cannot coerce safely"
           [ "from type" <+> pp tt1
           , "to type" <+> pp tt2
           ]

  refl = unify tt1 (r,tt2) >> pure Solved

  fromInt
    | Lossy <- lossy =
      case tt2 of
        TVar _ -> pure Unsolved
        TCon {} -> nope
        Type y ->
          case y of
            TUInt _  -> pure Solved
            TSInt _  -> pure Solved
            TInteger -> pure Solved
            _        -> nope

    | otherwise = refl



  fromUInt x =
    case tt2 of
      TVar _ -> pure Unsolved
      TCon {} -> nope
      Type t2 ->
        case t2 of
          TInteger -> pure Solved

          TUInt y
            | Lossy <- lossy -> pure Solved
            | otherwise ->
              case (x,y) of
                (Type (TNum s1), Type (TNum s2))
                  | s1 <= s2  -> pure Solved
                  | otherwise -> nope
                _ -> pure Unsolved

          TSInt y
            | Lossy <- lossy -> pure Solved
            | otherwise ->
              case (x,y) of
                (Type (TNum s1), Type (TNum s2))
                  | s1 < s2   -> pure Solved
                  | otherwise -> nope
                _ -> pure Unsolved

          _  -> nope


  fromSInt x =
    case tt2 of
      TVar _ -> pure Unsolved
      TCon {} -> nope
      Type t2 ->
        case t2 of

          TInteger -> pure Solved

          TUInt _
            | Lossy <- lossy -> pure Solved

          TSInt y
            | Lossy <- lossy -> pure Solved
            | otherwise ->
              case (x,y) of
                (Type (TNum s1), Type (TNum s2))
                   | s1 <= s2       -> pure Solved
                   | otherwise      -> nope
                _ -> pure Unsolved

          _ -> nope




validLiteral :: (STCMonad m, HasRange r) =>
                r -> Integer -> Type -> m CtrStatus
validLiteral r i ty =
  case ty of
    TVar _ -> pure Unsolved
    TCon {} -> nope
    Type t ->
      case t of
        TInteger -> pure Solved
        TUInt n
          | i < 0  -> nope
          | otherwise ->
            case n of
              Type (TNum x) | 0 <= i && i < 2 ^ x -> pure Solved
                            | otherwise -> nope
              _ -> pure Unsolved
        TSInt n ->
          case n of
            Type (TNum x) | x >= 0
                          , let y = 2 ^ (x - 1)
                          , negate y <= i && i < y -> pure Solved
                          | otherwise -> nope
            _ -> pure Unsolved
        _ -> nope

  where
  nope = reportError r ("Literal" <+> pp i <+> "does not fit in" <+>
                                backticks (pp ty))


lookupTyDefInst :: (STCMonad m, HasRange r) =>
  r -> TCTyName -> [Type] -> m (Maybe TCTyDef)
lookupTyDefInst r x ts =
  do mb <- lookupTypeDef x
     case mb of
       Nothing -> pure Nothing
       Just def ->
         do let as = tctyParams def
                expect = length as
                have = length ts
            when (expect /= have)
               $ reportDetailedError r "Incorect number of parameters."
                    [ "Expected:" <+> int expect
                    , "Given:" <+> int have
                    ]
            let su = Map.fromList (zip as ts)
            pure (Just (apSubstT su (tctyDef def)))

checkFields :: (STCMonad m, HasRange r) =>
              r -> TCTyName -> Map Label Type -> [(Label,Located Type)] -> m ()
checkFields r x dfs fs =
  case fs of
    [] -> case Map.keys dfs of
            []  -> pure ()
            mis -> reportDetailedError r "Missing fields" (map pp mis)

    (f,t) : more ->
       case Map.updateLookupWithKey (\_ _ -> Nothing) f dfs of
         (Just t1,df1) -> do unify t1 (t,thingValue t)
                             checkFields r x df1 more
         (Nothing,_) -> reportError t
                          ("Type" <+> backticks (pp x) <+>
                           "does not have field" <+> backticks (pp f))


isTyDef :: (STCMonad m, HasRange r) =>
              r -> TyDef -> Type -> [(Label,Located Type)] -> m CtrStatus
isTyDef r ty t fs0 =
  case t of
    TVar _ -> pure Unsolved
    TCon c ts ->
      do mb <- lookupTyDefInst r c ts
         case mb of
           Nothing -> pure Unsolved -- Add definition, if no params?
           Just def ->
             case def of
               TCTyStruct dfs
                 | StructDef <- ty -> do checkFields r c (Map.fromList dfs) fs0
                                         pure Solved
                 | otherwise -> reportError r "Structure used as union."
               TCTyUnion dfs
                 | UnionDef <- ty -> do checkFields r c (Map.fromList dfs) fs0
                                        pure Solved
                 | otherwise -> reportError r "Union used a structure"

    Type _ -> reportDetailedError r "Expected a structure."
                  [ "Actual type:" <+> pp t ]


isNamed :: (STCMonad m, HasRange r) => r -> Type -> m CtrStatus
isNamed r ty =
  case ty of
    TCon {} -> pure Solved
    TVar {} -> pure Unsolved
    Type {} -> reportDetailedError r "Expected a named type"
                  [ pp ty <+> "is not." ]


-- | Try to solve a constraint.
-- Fails if the constraint is known to be unsolvable.
-- May bind variables.
-- Does not generate new constraints.
solveConstraint :: STCMonad m => Located Constraint -> m CtrStatus
solveConstraint lctr =
  case thingValue lctr of
    Numeric t          -> isNumeric lctr t
    HasStruct t l fty  -> hasStruct lctr t l fty
    TyDef ty _ t fs    -> isTyDef lctr ty t fs
    HasUnion  t l fty  -> hasUnion  lctr t l fty
    Coerce lossy t1 t2 -> isCoercible lctr lossy t1 t2
    Literal n t        -> validLiteral lctr n t
    CAdd x1 x2 x3      -> isAdd lctr x1 x2 x3
    IsNamed t          -> isNamed lctr t
    Traversable t      -> isTraversable lctr t
    Mappable t s       -> isMappable lctr t s
    ColElType c e      -> isColElType lctr c e
    ColKeyType c e     -> isColKeyType lctr c e


--------------------------------------------------------------------------------

class Unify t where
  unify :: (STCMonad m, HasRange r) => t -> (r,Type) -> m ()

instance Unify Type where
  unify t1 (r,t2) = unify2 Nothing r t1 t2

instance HasRange r => Unify (r,Type) where
  unify (r1,t1) (r2,t2) = unify2 (Just (range r1)) r2 t1 t2



unify2 ::
  (STCMonad m, HasRange r) =>
    Maybe SourceRange -> r -> Type -> Type -> m ()
unify2 r s t1' t2' =
  do t1 <- zonkT t1'
     t2 <- zonkT t2'
     let nope = reportDetailedError s "Failed to match types:"
                                       [ mbLoc r <+> pp t1
                                       , mbLoc (r >> Just (range s)) <+> pp t2
                                       ]

     case (t1,t2) of
       (TVar x, _)                -> bindVar (fromMaybe (range x) r) s x t2
       (_, TVar x)                -> bindVar s (fromMaybe (range x) r) x t1
       (TCon c xs, TCon d ys) | c == d -> unifyMany r s xs ys
       (Type tf1, Type tf2) ->
          case (tf1,tf2) of
            (TGrammar x, TGrammar y) -> unify2 r s x y
            (TFun x1 x2, TFun y1 y2) ->
               do unify2 r s x1 y1
                  unify2 r s x2 y2
            (TStream,    TStream)      -> pure ()
            (TByteClass, TByteClass)   -> pure ()
            (TNum x, TNum y) | x == y  -> pure ()
            (TUInt x, TUInt y)         -> unify2 r s x y
            (TSInt x, TSInt y)         -> unify2 r s x y
            (TUnit, TUnit)             -> pure ()
            (TInteger,TInteger)        -> pure ()
            (TBool, TBool)             -> pure ()
            (TMaybe x, TMaybe y)       -> unify2 r s x y
            (TArray x, TArray y)       -> unify2 r s x y
            (TMap k1 v1, TMap k2 v2)   -> do unify2 r s k1 k2
                                             unify2 r s v1 v2
            _ -> nope
       _ -> nope
     where
     mbLoc m = case m of
                 Nothing -> empty
                 Just l  -> "At" <+> pp l <.> ","

unifyMany :: (STCMonad m, HasRange r) =>
  Maybe SourceRange -> r -> [Type] -> [Type] -> m ()
unifyMany r s xs ys =
  case (xs,ys) of
    ([],[])     -> pure ()
    (a:as,b:bs) -> unify2 r s a b >> unifyMany r s as bs
    _ -> reportError s "Arity mismatch when matching types"


bindVar ::
  (STCMonad m, HasRange r, HasRange s) =>
  r -> s -> TVar -> Type -> m ()
bindVar r s x t =
  case t of
    TVar y
       | x == y -> pure ()
    _  | tvarKind x == kindOf t ->
          if x `Set.member` freeTVS t
            then reportDetailedError s
                    "Invlid recursive type"
                    [ "Type" <+> pp x <+> "occurs in"
                    , "Type" <+> pp t <+> ", see" <+> pp (range r)
                    ]
            else addTVarDef x t
       | otherwise -> reportDetailedError r "Kind mismatch"
                         [ pp (tvarKind x), pp (kindOf t)  ]

simplifyConstraints :: STCMonad m => m [Located Constraint]
simplifyConstraints =
  do su <- getTypeSubst
     go [] su =<< removeConstraints
  where
  go notYet su todo =
    case todo of
      [] -> case notYet of
              [] -> pure []
              _  -> goSame [] su notYet

      c : cs ->
        do let c1 = apSubstT su c
           res <- solveConstraint c1
           case res of
             Solved   -> go notYet su cs
             Unsolved -> go (c1 : notYet) su cs

  goSame notYet su todo =
    case todo of
      [] -> do su1 <- getTypeSubst
               if Map.size su < Map.size su1
                  then go [] su1 notYet
                  else tryAddDefVar [] notYet
      c : cs ->
        do known <- checkKnown c notYet
           if known then goSame notYet su cs
                    else goSame (c : notYet) su cs

  tryAddDefVar notYet todo =
    case todo of
      c : more ->

        case thingValue c of
          TyDef ty (Just suggest) theTy fs ->

            case (suggest, theTy) of
              (TCTyAnon nm _, TCon tcon@(TCTy nm1) [])
                 | nm == nm1 -> defTy tcon

              (_, TVar _) ->
                 do unify theTy (c,tCon suggest [])
                    defTy suggest

              _ -> tryAddDefVar (c : notYet) more

             where
             defTy tcon =
               do let fields = [ (f,thingValue t) | (f,t) <- fs ]
                  newTypeDef tcon
                    case ty of
                      StructDef -> TCTyStruct fields
                      UnionDef  -> TCTyUnion  fields
                  su <- getTypeSubst
                  go [] su (notYet ++ more)

          _ -> tryAddDefVar (c : notYet) more

      [] -> pure notYet

  checkKnown c notYet =
    case notYet of
      [] -> pure False
      o : more ->
        do yes <- isSameCtr c o
           if yes then pure True else checkKnown c more


