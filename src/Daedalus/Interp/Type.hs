module Daedalus.Interp.Type where

import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Map as Map
import Data.List(foldl')
import Data.Bits(shiftR,shiftL,(.|.))

import Daedalus.PP
import Daedalus.Panic
import qualified Daedalus.BDD as BDD

import Daedalus.Value

import Daedalus.Interp.Env
import Daedalus.Type.AST
import qualified Daedalus.Type.AST as AST


evalType :: Env -> Type -> TValue
evalType env ty =
  case ty of
    TVar x -> lkpTy x
    TCon c []
      | Just decl <- Map.lookup c (tyDecls env)
      , let name = Text.pack (show (pp (tctyName decl)))
      , Just u <- tctyBD decl -> evalBitdataType env name u (tctyDef decl)
    TCon {} -> TVOther
    Type t0 ->
      case t0 of
        TGrammar _ -> TVOther
        TFun _ _   -> TVOther
        TStream    -> TVOther
        TByteClass -> TVOther
        TNum n     -> TVNum (fromIntegral n) -- wrong for very large sizes.
        TUInt t    -> TVUInt (tvInt t)
        TSInt t    -> TVSInt (tvInt t)
        TInteger   -> TVInteger
        TMap {}    -> TVMap
        TArray {}  -> TVArray
        TBool      -> TVOther
        TFloat     -> TVFloat
        TDouble    -> TVDouble
        TUnit      -> TVOther
        TMaybe {}  -> TVOther
        TBuilder {}-> TVOther

  where
  lkpTy x = case Map.lookup x (tyEnv env) of
              Just tv -> tv
              Nothing -> panic "evalType"
                            [ "undefined type vairalbe"
                            , show (pp x)
                            ]

  tvInt t = case evalType env t of
              TVNum n -> n
              it      -> panic "evalType.tvInt" [ "Expected a number"
                                                , "Got: " ++ show (pp it)
                                                ]

-- XXX: This reavaluates types over and over againg.  It might be
-- better to evalute bitdata types in the environment once instead,
-- and store them in the environemtn.
evalBitdataType :: Env -> Text -> BDD.Pat -> TCTyDef -> TValue
evalBitdataType env name u def =
  case def of

    TCTyStruct ~(Just bd) _ ->
      let fs = AST.bdFields bd
      in
      TVBDStruct BDStruct
        { bdName = name
        , bdWidth  = BDD.width u
        , bdGetField =
            let mp =
                  Map.fromList
                    [ (l, inField (AST.bdOffset f) ty)
                    | f <- fs
                    , AST.BDData l ty <- [AST.bdFieldType f]
                    ]
            in \l -> case Map.lookup l mp of
                       Just v  -> v
                       Nothing -> panic "evalBitdataType"
                                    [ "Missing field: " ++ showPP l ]
        , bdStruct   = \fvs -> foldl' (outField fvs) 0 (AST.bdFields bd)
        , bdValid    = BDD.willMatch u
        , bdFields   = [ l | AST.BDData l _ <- map AST.bdFieldType fs ]
        }

    TCTyUnion fs ->
      let mp = Map.fromList [ (l, (evalType env t,p)) | (l,(t,Just p)) <- fs ]
      in
      TVBDUnion BDUnion
        { bduName  = name
        , bduWidth = BDD.width u
        , bduValid = BDD.willMatch u
        , bduGet   =
          \l -> case Map.lookup l mp of
                  Just (t,_) -> vFromBits t
                  Nothing -> panic ("bduGet@" ++ Text.unpack name)
                                   ["Unknown constructor: " ++ showPP l]
        , bduMatches =
          \l -> BDD.willMatch
                case Map.lookup l mp of
                  Just (_,p) -> p
                  Nothing -> panic ("bduMatches@" ++ Text.unpack name)
                                   ["Unknown constructor: " ++ showPP l]
        , bduCases = map fst fs
        }


  where
  inField off t = \i -> vFromBits (evalType env t) (i `shiftR` off)
  outField fs w f =
    shiftL w (AST.bdWidth f) .|.
    case AST.bdFieldType f of
      AST.BDWild  -> 0
      AST.BDTag n -> n
      AST.BDData l _ ->
        vToBits
        case lookup l fs of
          Just fv -> fv
          Nothing -> panic "outField" ["Missing field value", showPP l ]
--------------------------------------------------------------------------------


