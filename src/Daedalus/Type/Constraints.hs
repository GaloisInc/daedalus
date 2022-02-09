{-# Language OverloadedStrings, BlockArguments, FlexibleInstances #-}
module Daedalus.Type.Constraints (simplifyConstraints, unify) where

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe(fromMaybe)
import Control.Monad(when,unless)

import Daedalus.PP
import qualified Daedalus.BDD as BDD
import Daedalus.SourceRange

import Daedalus.AST
import Daedalus.Type.AST
import Daedalus.Type.Monad
import Daedalus.Type.Subst


data CtrStatus = Solved | Unsolved
  deriving Show

isImpliedBy ::
  STCMonad m => Located Constraint -> Located Constraint -> m Bool
cNew `isImpliedBy` cOld =
  case (thingValue cNew, thingValue cOld) of
    (Integral x, Integral y) | x == y -> pure True
    (Arith x, Arith y) | x == y -> pure True


    (Arith x, Integral y) | x == y -> pure True
    (Arith x, FloatingType y) | x == y -> pure True

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

    Type TArray {}  -> unify tSize (r,e) *> pure Solved
    Type (TMap a _) -> unify a     (r,e) *> pure Solved
    Type TInteger   -> unify t     (r,e) *> pure Solved
    Type TUInt{}    -> unify t     (r,e) *> pure Solved

    _ -> reportDetailedError r "Invalid collection type"
              [ "Collection:" <+> pp t
              , "Key type:" <+> pp e
              ]



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


isIntegral :: (STCMonad m, HasRange r) => r -> Type -> m CtrStatus
isIntegral r ty =
  case ty of
    Type TInteger   -> pure Solved
    Type (TUInt _)  -> pure Solved
    Type (TSInt _)  -> pure Solved
    TVar _          -> pure Unsolved
    _               -> reportDetailedError r "Not an integral type."
                          [ "Type:" <+> pp ty ]


isArith :: (STCMonad m, HasRange r) => r -> Type -> m CtrStatus
isArith r ty =
  case ty of
    Type TInteger   -> pure Solved
    Type (TUInt _)  -> pure Solved
    Type (TSInt _)  -> pure Solved
    Type TFloat     -> pure Solved
    Type TDouble    -> pure Solved
    TVar _          -> pure Unsolved
    _               -> reportDetailedError r "Type does not support arithmetic."
                          [ "Type:" <+> pp ty ]





isFloatingType :: (STCMonad m, HasRange r) => r -> Type -> m CtrStatus
isFloatingType r ty =
  case ty of
    Type TFloat  -> pure Solved
    Type TDouble -> pure Solved
    TVar _       -> pure Unsolved
    _            -> reportDetailedError r "Not a floating point type."
                          [ "Type:" <+> pp ty ]




hasStruct :: (STCMonad m, HasRange r) =>
            r -> Type -> Label -> Type -> m CtrStatus
hasStruct r ty l fty =
  case ty of
    TVar _ -> pure Unsolved

    TCon c ts ->
      do mb <- lookupTypeDefMaybe c
         case mb of
           Nothing -> pure Unsolved
           Just td -> case tctyDef td of
                        TCTyStruct _ fs | Just fty1 <- lookup l fs ->
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
           Just (td,inferring) ->
              case tctyDef td of
                TCTyUnion fs ->
                  case lookup l fs of
                    Just (fty1,_) ->
                      do let su = Map.fromList (zip (tctyParams td) ts)
                         unify (apSubstT su fty1) (r,fty)
                         pure Solved
                    Nothing -> if inferring then pure Unsolved
                                            else doErr
                _ -> doErr

    _ -> doErr
  where
  doErr = reportDetailedError r
            ("Type does not have tag" <+> backticks (pp l))
            [ "Problem type:" <+> pp ty ]


isCoercible :: (STCMonad m, HasRange r) =>
               r -> Lossy -> Type -> Type -> m CtrStatus
isCoercible r lossy tt1 tt2 =
  case tt1 of
    TVar _   ->
      case tt2 of
        TCon tc [] ->
          do mb <- isBitData tc
             case mb of
               Nothing -> refl
               Just bd ->
                 -- note that this does not allow for reflexivity coercions
                 -- or direct coercions from one bitdata to another.
                 -- rather, the user would have to first coerce to `uint X`
                 do let n = toInteger (BDD.width bd)
                    unify (tUInt (tNum n)) (r,tt1)
                    if BDD.willAlwaysMatch bd || lossy == Dynamic
                      then pure Solved
                      else nope
        TCon {} -> refl
        _ -> pure Unsolved


    TCon tc [] ->
      do mb <- isBitData tc
         case mb of
           Nothing -> refl
           Just bd ->
             do let n = toInteger (BDD.width bd)
                unify (tUInt (tNum n)) (r,tt2)
                pure Solved
    TCon {} -> refl
    Type t1 ->
      case t1 of
        TInteger  -> fromInt
        TUInt x   -> fromUInt x
        TSInt x   -> fromSInt x
        _         -> refl
  where
  nope =
    do t1 <- zonkT tt1
       t2 <- zonkT tt2
       reportDetailedError r ("Cannot coerce" <+> how)
           [ "from type" <+> pp t1
           , "to type" <+> pp t2
           ]

  how = case lossy of
          NotLossy -> "safely"
          _        -> empty

  -- XXX: Perhaps these should be an error as it seems more likely that
  -- someone made a mistake if coercing from a type to itself...
  refl = unify tt1 (r,tt2) >> pure Solved

  fromInt
    | NotLossy <- lossy = refl
    | otherwise =
      case tt2 of
        TVar _ -> pure Unsolved
        TCon {} -> nope
        Type y ->
          case y of
            TUInt _  -> pure Solved
            TSInt _  -> pure Solved
            TInteger -> pure Solved
            TFloat   -> pure Solved
            _        -> nope


  fromUInt x =
    case tt2 of
      TVar _ -> pure Unsolved
      TCon tc args ->
        do unless (null args) nope
           mb <- isBitData tc
           case mb of
             Just bd ->
                do unify x (r, tNum (toInteger (BDD.width bd)))
                   case lossy of
                     Dynamic -> pure Solved
                     NotLossy | BDD.willAlwaysMatch bd -> pure Solved
                     _ -> nope
             _ -> nope
      Type t2 ->
        case t2 of
          TInteger -> pure Solved

          TUInt y
            | NotLossy <- lossy ->
              case (x,y) of
                (Type (TNum s1), Type (TNum s2))
                  | s1 <= s2  -> pure Solved
                  | otherwise -> nope
                _ -> pure Unsolved

            | otherwise -> pure Solved

          TSInt y
            | NotLossy <- lossy ->
              case (x,y) of
                (Type (TNum s1), Type (TNum s2))
                  | s1 < s2   -> pure Solved
                  | otherwise -> nope
                _ -> pure Unsolved

            | otherwise -> pure Solved

          TFloat
            | NotLossy <- lossy ->
              case x of
                Type (TNum s) -> if s <= 24 then pure Solved else nope
                _             -> pure Unsolved
            | otherwise -> pure Solved

          TDouble
            | NotLossy <- lossy ->
              case x of
                Type (TNum s) -> if s <= 53 then pure Solved else nope
                _             -> pure Unsolved
            | otherwise -> pure Solved

          _  -> nope


  fromSInt x =
    case tt2 of
      TVar _ -> pure Unsolved
      TCon {} -> nope
      Type t2 ->
        case t2 of

          TInteger -> pure Solved

          TUInt _
            | Lossy  <- lossy -> pure Solved
            | Dynamic <- lossy -> pure Solved

          TSInt y
            | NotLossy <- lossy ->
              case (x,y) of
                (Type (TNum s1), Type (TNum s2))
                   | s1 <= s2       -> pure Solved
                   | otherwise      -> nope
                _ -> pure Unsolved

            | otherwise -> pure Solved

          TFloat
            | NotLossy <- lossy ->
              case x of
                Type (TNum s) -> if s <= 25 then pure Solved else nope
                _             -> pure Unsolved
            | otherwise -> pure Solved

          TDouble
            | NotLossy <- lossy ->
              case x of
                Type (TNum s) -> if s <= 54 then pure Solved else nope
                _             -> pure Unsolved
            | otherwise -> pure Solved



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

        TFloat ->
          let f = fromInteger i :: Float
          in if truncate f == i then pure Solved else nope

        TDouble ->
          let f = fromInteger i :: Double
          in if truncate f == i then pure Solved else nope

        _ -> nope

  where
  nope = reportError r ("Literal" <+> pp i <+> "does not fit in" <+>
                                backticks (pp ty))

lookupTyDefInst :: (STCMonad m, HasRange r) =>
  r -> TCTyName -> [Type] -> m (Maybe (TCTyDef, Bool))
lookupTyDefInst r x ts =
  do mb <- lookupTypeDef x
     case mb of
       Nothing -> pure Nothing
       Just (def,inferring) ->
         do let as     = tctyParams def
                expect = length as
                have   = length ts
            when (expect /= have)
               $ reportDetailedError r "Incorect number of parameters."
                    [ "Expected:" <+> int expect
                    , "Given:" <+> int have
                    ]
            let su = Map.fromList (zip as ts)
            pure (Just (apSubstT su (tctyDef def), inferring))


checkFields :: (STCMonad m, HasRange r) =>
              r -> TCTyName -> Map Label Type -> [(Label,Located Type)] -> m ()
checkFields r x dfs fs =    -- `dsf`, existing fields, `fs` new fields
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

isNamed :: (STCMonad m, HasRange r) => r -> Type -> m CtrStatus
isNamed r ty =
  case ty of
    TCon {} -> pure Solved
    TVar {} -> pure Unsolved
    Type {} -> reportDetailedError r "Expected a named type"
                  [ pp ty <+> "is not." ]

isStructCon ::
  (STCMonad m, HasRange r) => r -> Type -> [(Label,Located Type)] -> m CtrStatus
isStructCon r ty fs =
  case ty of
    TVar {} -> pure Unsolved

    TCon c ts ->
      do mb <- lookupTyDefInst r c ts
         case mb of
           Nothing -> pure Unsolved
           Just (def,_) ->
             case def of
               TCTyStruct _ dfs ->
                  do checkFields r c (Map.fromList dfs) fs
                     pure Solved
               TCTyUnion {} -> reportError r "Union used a structure"

    Type tc ->
      case tc of
        TUnit ->
          case fs of
            [] -> pure Solved
            (f,_) : _ ->
              reportError r
                ("Type {} does not have field" <+> backticks (pp f))

        _ -> reportDetailedError r "Type is not a structure"
              [ "Type:" <+> pp ty ]


isUnionCon ::
  (STCMonad m, HasRange r) => r -> Type -> Label -> Located Type -> m CtrStatus
isUnionCon r ty c t =
  case ty of
    TVar {} -> pure Unsolved
    TCon tc ts ->
      do mb <- lookupTyDefInst r tc ts
         case mb of
           Nothing -> pure Unsolved
           Just (def,inferring) ->
             case def of
               TCTyUnion dfs ->
                 do case lookup c dfs of
                      Just (t1,_) -> unify t1 (t, thingValue t)
                      Nothing
                        | inferring -> addCon tc c (thingValue t)
                        | otherwise ->
                          reportDetailedError r
                            "Union doesn't have this constructor"
                                [ "Type:" <+> pp tc
                                , "Constructor:" <+> pp c
                                ]

                    pure Solved
               TCTyStruct {} -> reportError r "Structure used as a union"
    Type _ -> reportDetailedError r "Type is not a union"
                  [ "Type:" <+> pp ty ]


-- | Try to solve a constraint.
-- Fails if the constraint is known to be unsolvable.
-- May bind variables.
-- May add extra constructors to union types that are being deifned.
-- Does not generate new constraints.
solveConstraint :: STCMonad m => Located Constraint -> m CtrStatus
solveConstraint lctr =
  case thingValue lctr of
    Integral t         -> isIntegral lctr t
    Arith t            -> isArith lctr t
    FloatingType t     -> isFloatingType lctr t
    HasStruct t l fty  -> hasStruct lctr t l fty
    StructCon _ t fs   -> isStructCon lctr t fs
    UnionCon _ t c ft  -> isUnionCon lctr t c ft
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
            (TFloat, TFloat)           -> pure ()
            (TDouble, TDouble)         -> pure ()
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
     go False [] su =<< removeConstraints
  where

  go anySolved notYet su todo =
    case todo of
      [] -> case notYet of
              [] -> pure []
              _ -> do su1 <- getTypeSubst
                      if anySolved then go False [] su1 notYet
                                   else goSame [] su notYet

      c : cs ->
        do let c1 = apSubstT su c
           res <- solveConstraint c1
           case res of
             Solved   -> go True notYet su cs
             Unsolved -> go anySolved (c1 : notYet) su cs

  goSame notYet su todo =
    case todo of
      [] -> do su1 <- getTypeSubst
               if Map.size su < Map.size su1
                  then go False [] su1 notYet
                  else tryAddDefVar [] notYet
      c : cs ->
        do known <- checkKnown c notYet
           if known then goSame notYet su cs
                    else goSame (c : notYet) su cs

  tryAddDefVar notYet todo =
    case todo of
      ctr : more
        | StructCon suggested ty fs <- thingValue ctr ->
          case fs of

            -- Empty struct types just get the unit type
            [] -> do unify ty (ctr, tUnit)
                     su <- getTypeSubst
                     go False [] su (notYet ++ more)

            _  -> chooseName suggested ty
                     (TCTyStruct Nothing [ (f, thingValue lt) | (f,lt) <- fs ])
        | UnionCon suggested ty c t <- thingValue ctr ->
          chooseName suggested ty
            (TCTyUnion [ (c, (thingValue t,Nothing)) ])
        where
        chooseName suggested theTy def =
          case (suggested, theTy) of

            {- This is to handle cases like this:
            def F = { x = UInt8 } : F
            In this case, we get a constraint: { x : uint 8 } in F
            but `F` is not defined, so we need to add a definition.
            Note that we have to be careful to only define it once,
            here this is ensured because `constructor` constraints only
            get to here if there was no definition, and we immediatly
            restart the process after the definition. -}
            (TCTyAnon nm _, TCon tcon@(TCTy nm1) [])
               | nm == nm1 -> defTy tcon def

            (_, TVar {}) -> do unify theTy (ctr, TCon suggested [])
                               defTy suggested def
            _ -> tryAddDefVar (ctr : notYet) more

        defTy tcon def =
          do newTypeDef tcon def
             su <- getTypeSubst
             go False [] su (notYet ++ more)

      ctr : more -> tryAddDefVar (ctr : notYet) more

      [] -> pure notYet

  checkKnown c notYet =
    case notYet of
      [] -> pure False
      o : more ->
        do yes <- c `isImpliedBy` o
           if yes then pure True else checkKnown c more


