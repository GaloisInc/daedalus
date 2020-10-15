{-# Language GADTs, BlockArguments, OverloadedStrings, NamedFieldPuns #-}
{-# Language RecordWildCards #-}
module Daedalus.Type.DeadVal where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Maybe(catMaybes)
import Control.Monad(guard,zipWithM,liftM,ap,forM)
import Data.Parameterized.Some

import Daedalus.SourceRange
import Daedalus.PP
import Daedalus.Panic(panic)

import Daedalus.Type.AST
import Daedalus.Type.Free

{-
XXX: It might make more sense to that after specialization,
  1. we would handle function parameters better,
  2. the whole thing would be simpler
  3. we'd generate less code, as we'd only have the functions
     that are actually used.
-}

deadValModules :: [TCModule SourceRange] -> [TCModule SourceRange]
deadValModules = go [] Map.empty
  where
  go done known ms =
    case ms of
      [] -> reverse done
      m : more ->
        case deadValModule known m of
          (m1, newKnown) -> go (m1 : done) newKnown more

deadValModule ::
  Map Name ArgInfo ->
  TCModule SourceRange ->
  (TCModule SourceRange, Map Name ArgInfo)
deadValModule k0 mo = (mo { tcModuleDecls = topoOrder matchDecls }, allKnown)
  where
  (matchDecls,allKnown) = go [] k0 (concatMap recToList (tcModuleDecls mo))

  go :: [TCDecl SourceRange] ->
        Map Name ArgInfo ->
        [TCDecl SourceRange] ->
        ([TCDecl SourceRange], Map Name ArgInfo)
  go done known ds =
    case ds of
      [] -> (done, known)
      d : more ->
        case declareMatchFun known d of
          Just (d1,info) ->
            let newKnown = Map.insert (tcDeclName d) info known
            in go (cleanUpDecl newKnown d : d1 : done) newKnown more
          Nothing -> go (cleanUpDecl known d : done) known more


data ArgInfo = ArgInfo { matchFun       :: TCName Grammar
                       , passArg        :: [Bool]
                       , extraMatchArgs :: [Int]
                       }

newtype NoFunM a = NFM (RO -> RW -> (a,RW))

instance Functor NoFunM where
  fmap = liftM

instance Applicative NoFunM where
  pure a = NFM \_ s -> (a,s)
  (<*>)  = ap

instance Monad NoFunM where
  NFM m >>= f = NFM \r s -> case m r s of
                              (a,s1) -> let NFM m1 = f a
                                        in m1 r s1

data RO = RO
  { roMatchFuns    :: Map Name ArgInfo
  , roChangeParams :: Bool
  }

newtype RW = RW
  { matchArgs :: Map (TCName Grammar) (TCName Grammar)
  }

runNoFunM :: Bool -> Map Name ArgInfo -> NoFunM a -> (a, RW)
runNoFunM changePs r (NFM m) = m RO { roChangeParams = changePs
                                    , roMatchFuns = r
                                    }
                                 RW { matchArgs = Map.empty }

attempt :: NoFunM (Maybe a) -> NoFunM a -> NoFunM a
attempt (NFM m1) (NFM m2) = NFM \r s ->
  case m1 r s of
    (Just a, s1) -> (a, s1)
    (Nothing,_)  -> m2 r s

newMParam ::
  HasRange r => r -> TCName Grammar -> NoFunM (TC SourceRange Grammar)
newMParam r x = NFM \ro s ->
  if roChangeParams ro
    then case Map.lookup x (matchArgs s) of
           Just y  -> (exprAt r (TCVar y), s)
           Nothing -> let y = TCName { tcNameCtx = AGrammar
                                     , tcType    = tGrammar tUnit
                                     , tcName    = modifyName (tcName x)
                                     }
                     in ( exprAt r (TCVar y)
                        , s { matchArgs = Map.insert x y (matchArgs s) }
                        )
    else ( mkDo r Nothing (exprAt r (TCVar x)) (noSemPure r)
         , s)


modifyName :: Name -> Name
modifyName nm = case nameScope nm of
                  Local i -> nm { nameScope = Local (newI i) }
                  ModScope m i -> nm { nameScope = ModScope m (newI i) }
                  _ -> panic "newMParam" ["Not a local name"]
  where
  newI i = "_" <> i

lookupMatchFun :: TCName Grammar -> NoFunM (Maybe ArgInfo)
lookupMatchFun x = NFM \r s -> (Map.lookup (tcName x) (roMatchFuns r), s)

--------------------------------------------------------------------------------



type Info = Set (Some TCName)


cleanUpDecl :: Map Name ArgInfo -> TCDecl SourceRange -> TCDecl SourceRange
cleanUpDecl mfs dcl =
  case dcl of
    TCDecl { tcDeclCtxt = AGrammar, tcDeclDef, .. } ->
      case tcDeclDef of
        ExternDecl _ -> dcl
        Defined d ->
          case runNoFunM False mfs (mbSem d) of
            ((def,_),_) -> TCDecl { tcDeclDef = Defined def
                                  , tcDeclCtxt = AGrammar, .. }
    _ -> dcl


declareMatchFun ::
  Map Name ArgInfo -> TCDecl SourceRange -> Maybe (TCDecl SourceRange, ArgInfo)
declareMatchFun mfs dcl =
  case dcl of
    TCDecl { tcDeclCtxt = AGrammar, tcDeclDef, tcDeclAnnot } ->
      let newName = TCName { tcNameCtx = AGrammar
                           , tcType    = tGrammar tUnit
                           , tcName    = modifyName (tcDeclName dcl)
                           }

      in
      case tcDeclDef of

        -- XXX: It seems that we could just leave this as is
        -- and just ignore the argments at the call site...
        ExternDecl _ -> Just (newDecl, newInfo)
           where
           newDecl = TCDecl
                      { tcDeclName     = tcName newName
                      , tcDeclTyParams = tcDeclTyParams dcl
                      , tcDeclCtrs     = tcDeclCtrs dcl
                      , tcDeclParams   = tcDeclParams dcl
                      , tcDeclDef      = Defined newDef
                      , tcDeclCtxt     = AGrammar
                      , tcDeclAnnot    = tcDeclAnnot
                      }
           newInfo = ArgInfo { matchFun = newName
                             , passArg  = map (const True) (tcDeclParams dcl)
                             , extraMatchArgs = []
                             }
           loc     = range (tcDeclName dcl)
           newDef  = mkDo loc Nothing theCall (noSemPure loc)
           theCall = exprAt loc
                   $ TCCall oldName (map TVar (tcDeclTyParams dcl))
                                    (map mkArg (tcDeclParams dcl))
           mkArg p = case p of
                       GrammarParam x -> GrammarArg $ exprAt loc $ TCVar x
                       ClassParam   x -> ClassArg   $ exprAt loc $ TCVar x
                       ValParam     x -> ValArg     $ exprAt loc $ TCVar x

           oldName = TCName { tcNameCtx = AGrammar
                            , tcName    = tcDeclName dcl
                            , tcType    = typeOf tcDeclDef
                            }


        Defined d ->
          case runNoFunM True mfs (noSem' d) of
            ((def,vs),rw) -> Just (newDecl, newInfo)
              where
              pused p = pname p `Set.member` vs

              pMap = Map.fromList $ zip (map pname (tcDeclParams dcl)) [ 0 .. ]

              pname p = case p of
                          ClassParam x   -> Some x
                          ValParam x     -> Some x
                          GrammarParam x -> Some x

              newDecl = TCDecl
                          { tcDeclName     = tcName newName
                          , tcDeclTyParams = tcDeclTyParams dcl
                          , tcDeclCtrs     = tcDeclCtrs dcl
                          , tcDeclParams   = newParams
                          , tcDeclDef      = Defined def
                          , tcDeclCtxt     = AGrammar
                          , tcDeclAnnot    = tcDeclAnnot
                          }

              newParams = [ p | p <- tcDeclParams dcl, pused p ] ++
                          [ GrammarParam nm | nm <- Map.elems (matchArgs rw) ]

              badPMap x = panic "pMap error" $
                            [ "Function: " ++ show (pp (tcDeclName dcl))
                            , "Param: " ++ show (pp x)
                            , "PMAP"
                            ] ++
                            [ "  " ++ show (pp v) ++ " := " ++ show y
                                  | (Some v,y) <- Map.toList pMap ]


              newInfo = ArgInfo
                          { matchFun = newName
                          , passArg = map pused (tcDeclParams dcl)
                          , extraMatchArgs =
                              [ case Map.lookup (Some x) pMap of
                                  Just a -> a
                                  Nothing -> badPMap x
                              | x <- Map.keys (matchArgs rw)
                              ]
                          }

    _ -> Nothing




-- | Look for opportunities within a grammar to use matching instead of parsing
mbSem :: TC SourceRange Grammar -> NoFunM (TC SourceRange Grammar, Info)
mbSem tc =
  case texprValue tc of
    TCFail e _ -> pure (tc, tcFree e)

    TCPure e -> pure (tc, tcFree e)

    TCDo mb m1 m2 ->
      do (m2',i2) <- mbSem m2
         (mb',m1',i1) <- case mb of
                           Just x | Some x `Set.member` i2 ->
                                do (m1',i1) <- mbSem m1
                                   pure (Just x,m1',i1)

                           _ -> do (m1',i1) <- noSem' m1
                                   pure (Nothing, m1',i1)
         pure (mkDo tc mb' m1' m2', i1 <> i2)

    TCLabel l m ->
      do (m',i) <- mbSem m
         pure (exprAt tc (TCLabel l m'),i)

    TCGetByte {}      -> pure (tc, tcFree tc)
    TCMatch {}        -> pure (tc, tcFree tc)
    TCGuard {}        -> pure (tc, tcFree tc)
    TCMatchBytes {}   -> pure (tc, tcFree tc)
    TCEnd             -> pure (tc, tcFree tc)
    TCOffset          -> pure (tc, tcFree tc)
    TCCurrentStream   -> pure (tc, tcFree tc)
    TCSetStream {}    -> pure (tc, tcFree tc)
    TCStreamLen _ e s -> pure (tc, tcFree e <> tcFree s)
    TCStreamOff _ e s -> pure (tc, tcFree e <> tcFree s)

    TCMapLookup {}    -> pure (tc, tcFree tc)
    TCMapInsert {}    -> pure (tc, tcFree tc)
    TCArrayIndex  {}  -> pure (tc, tcFree tc)

    TCCoerceCheck {}  -> pure (tc, tcFree tc)
    TCSelUnion {}     -> pure (tc, tcFree tc)
    TCSelJust {}      -> pure (tc, tcFree tc)
    TCVar {}          -> pure (tc, tcFree tc)

    TCChoice c ms t -> do (ms',is) <- unzip <$> mapM mbSem ms
                          pure (exprAt tc (TCChoice c ms' t), mconcat is)

    TCOptional c m ->
      do (m',i) <- mbSem m
         pure (exprAt tc (TCOptional c m'), i)

    TCMany s c b m ->
      do (m',i) <- mbSem m
         pure (exprAt tc (TCMany s c b m'), tcFree b <> i)

    TCFor lp  ->
      do (m',i) <- mbSem (loopBody lp)
         let bnd = Set.unions [ tcBinds (loopFlav lp),
                                tcBinds (loopKName lp),
                                tcBinds (loopElName lp) ]

         pure ( exprAt tc (TCFor lp { loopBody = m' })
              , tcFree (loopFlav lp) <>
                tcFree (loopCol lp) <>
                Set.difference i bnd
              )

    TCCall f ts as ->
      do (as',is) <- unzip <$> forM as \a ->
                       case a of
                         GrammarArg m ->
                            do (m',i) <- mbSem m
                               pure (GrammarArg m', i)
                         ClassArg {} -> pure (a, tcFree a)
                         ValArg {}   -> pure (a, tcFree a)

         pure (exprAt tc (TCCall f ts as'), mconcat is)

    TCErrorMode m p -> do (p', vs) <- mbSem p
                          pure (exprAt tc (TCErrorMode m p'), vs)

-- | Rewrite productions to avoid constructing a semantic value.
noSem' :: TC SourceRange Grammar -> NoFunM (TC SourceRange Grammar, Info)
noSem' tc =
  case texprValue tc of

     TCFail e _ -> pure (exprAt tc (TCFail e tUnit), tcFree e)

     TCPure _ -> pure (noSemPure tc, mempty)

     TCDo mb m1 m2 ->
       do (m2',i2) <- noSem' m2
          case mb of
            Just x | Some x `Set.member` i2 ->
              do (m1',i1) <- mbSem m1
                 pure (mkDo tc (Just x) m1' m2', i1 <> i2)

            _ -> do (m1',i1) <- noSem' m1
                    pure (mkDo tc Nothing m1' m2', i1 <> i2)

     TCLabel l m ->
       do (m',i) <- noSem' m
          pure (exprAt tc (TCLabel l m'), i)

     TCGetByte _      -> pure (exprAt tc (TCGetByte NoSem),   mempty)
     TCMatch _ c      -> pure (exprAt tc (TCMatch   NoSem c), tcFree c)
     TCGuard {}       -> pure (tc, tcFree tc)
     TCMatchBytes _ v -> pure (exprAt tc (TCMatchBytes NoSem v), tcFree v)

     TCChoice c ms _ ->
       do (ms',is) <- unzip <$> mapM noSem' ms
          pure (noSemUni tc c ms', mconcat is)

     TCOptional c m ->
       do (m', i) <- noSem' m
          pure (noSemUni tc c [ m', noSemPure tc ], i)

     TCMany _ c bs m ->
       do (m', i) <- noSem' m
          pure (exprAt tc (TCMany NoSem c bs m'), tcFree bs <> i)

     TCEnd    -> pure (tc, mempty)
     TCOffset -> pure (noSemPure tc, mempty)
     TCCurrentStream -> pure (noSemPure tc, mempty)
     TCSetStream s   -> pure ( exprAt tc (TCSetStream s), tcFree tc)

     TCStreamLen _ e s -> pure ( exprAt tc (TCStreamLen NoSem e s)
                               , tcFree e <> tcFree s
                               )
     TCStreamOff _ e s -> pure ( exprAt tc (TCStreamLen NoSem e s)
                               , tcFree e <> tcFree s
                               )

     TCMapLookup _ k mp ->
      pure (exprAt tc (TCMapLookup NoSem k mp), tcFree k <> tcFree mp)

     TCMapInsert _ k v mp ->
      pure ( exprAt tc (TCMapInsert NoSem k v mp)
           , tcFree k <> tcFree v <> tcFree mp )

     TCArrayIndex _ e ix ->
      pure (exprAt tc (TCArrayIndex NoSem e ix), tcFree e <> tcFree ix)

     TCCoerceCheck _ t1 t2 v ->
      pure ( exprAt tc (TCCoerceCheck NoSem t1 t2 v), tcFree v )

     TCSelUnion _ v l t ->
      pure (exprAt tc (TCSelUnion NoSem v l t), tcFree v)

     TCSelJust _ v t ->
      pure (exprAt tc (TCSelJust NoSem v t), tcFree v)

     TCErrorMode m p -> do (p',is) <- noSem' p
                           pure (exprAt tc (TCErrorMode m p'), is)

     TCFor lp ->
       attempt
         do (m', i) <- noSem' (loopBody lp)
            case loopFlav lp of
              -- map (i,x in xs) e
              -- ~>
              -- for (i,x in xs) e'
              LoopMap -> pure Nothing -- XXX

              Fold x _s ->
                pure do guard (not (Some x `Set.member` i)) -- state unused
                        let v' = x { tcType = tUnit }
                            lp' = lp { loopFlav = Fold v' (exprAt tc TCUnit)
                                     , loopBody = m'
                                     , loopType = tGrammar tUnit
                                     }
                        pure ( exprAt tc (TCFor lp')
                             , tcFree (loopCol lp) <> Set.difference i bnd
                             )

         do (m'', i') <- mbSem (loopBody lp)
            pure ( exprAt tc (TCFor lp { loopBody = m'' })
                 , tcFree (loopFlav lp) <> tcFree (loopCol lp) <>
                   Set.difference i' bnd
                 )

       where bnd = Set.unions [ tcBinds (loopFlav lp)
                              , tcBinds (loopKName lp)
                              , tcBinds (loopElName lp)
                              ]

     TCVar x ->
       case typeOf x of
         Type (TGrammar (Type TUnit)) -> pure (tc, mempty)
         _ -> do e <- newMParam tc x
                 pure (e, mempty)

     TCCall f ts as ->
       do mbArgInfo <- lookupMatchFun f
          case mbArgInfo of
            Nothing ->
              pure ( mkDo tc Nothing tc (noSemPure tc)
                   , if isLocalName (tcName f)
                         then Set.singleton (Some f) <> tcFree as
                         else tcFree as
                   )

            Just argInfo ->
              do let argMap = Map.fromList (zip [ 0 .. ] as)
                     extraArg i =
                        case Map.lookup i argMap of
                          Just (GrammarArg e) -> do (e',i') <- noSem' e
                                                    pure (GrammarArg e', i')
                          _ -> error "missing/wrong arg"

                     normalArg e yes
                       | yes = case e of
                                 GrammarArg e' -> do (e'',i) <- mbSem e'
                                                     pure (Just (GrammarArg e'',i))
                                 ValArg e' -> pure (Just (ValArg e', tcFree e'))
                                 ClassArg e' -> pure (Just (ClassArg e', tcFree e'))

                       | otherwise = pure Nothing

                 (as1,is1) <- unzip . catMaybes <$>
                                        zipWithM normalArg as (passArg argInfo)
                 (as2,is2) <- unzip <$> mapM extraArg (extraMatchArgs argInfo)
                 pure ( exprAt tc (TCCall (matchFun argInfo) ts (as1 ++ as2))
                      , mconcat (is1 ++ is2)
                      )




noSemUni ::
  HasRange r =>
  r ->
  Commit ->
  [TC SourceRange Grammar] ->
  TC SourceRange Grammar
noSemUni tc c ms = exprAt tc (TCChoice c ms tUnit)

noSemPure :: HasRange r => r -> TC SourceRange Grammar
noSemPure tc = exprAt tc (TCPure (exprAt tc TCUnit))

mkDo :: HasRange r =>
        r ->
        Maybe (TCName Value) ->
        TC SourceRange Grammar ->
        TC SourceRange Grammar ->
        TC SourceRange Grammar
mkDo r x m1 m2
  | Type (TGrammar (Type TUnit)) <- typeOf m1
  , TCPure e <- texprValue m2
  , TCUnit   <- texprValue e
    = m1

  | Nothing <- x
  , TCPure _ <- texprValue m1
    = m2

  | otherwise = exprAt r (TCDo x m1 m2)
