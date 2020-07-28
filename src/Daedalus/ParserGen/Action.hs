{-# Language GADTs, DataKinds, ExistentialQuantification, RecordWildCards #-}

module Daedalus.ParserGen.Action where

-- import Debug.Trace

import Control.Monad(guard)
import qualified Data.Map as Map
import Data.Word
import qualified Data.Vector as Vector
import Data.Maybe (fromJust)
import qualified Data.ByteString as BS
import Data.Text(Text)
import Data.Bits(shiftL,shiftR,(.|.),(.&.),xor)

import Daedalus.PP hiding (empty)

import Daedalus.AST (Name(..), ScopedIdent(..), BinOp(..), UniOp(..), Label)
import Daedalus.Normalise.AST
import Daedalus.Type.AST (WithSem(..))
import qualified Daedalus.Interp as Interp
import Daedalus.AST (ManyBounds(..), TypeF(..))
import RTS.ParserAPI(Input(..))

import qualified Daedalus.ParserGen.AST as PAST


type State = Int

data InputAction =
    ClssAct PAST.NCExpr
  | IEnd
  | IOffset
  | IGetByte
  | IMatchBytes PAST.NVExpr
  | CurrentStream
  | SetStream PAST.NVExpr
  | StreamLen PAST.NVExpr PAST.NVExpr
  | StreamOff PAST.NVExpr PAST.NVExpr

data ControlAction =
    BoundSetup (ManyBounds PAST.NVExpr)
  | BoundCheckSuccess
  | BoundIncr
  | BoundIsMore
  | ActivateFrame [PAST.NName]
  | DeactivateReady
  | Push PAST.NName [PAST.NVExpr] State
  | Pop State
  | ForInit PAST.NName PAST.NVExpr PAST.NName PAST.NVExpr
  | ForHasMore
  | ForNext
  | ForEnd
  | MapInit PAST.NName PAST.NVExpr
  | MapHasMore
  | MapNext
  | MapEnd

data SemanticAction =
    EnvFresh
  | ManyFreshList WithSem
  | ManyAppend    WithSem
  | EnvStore    (Maybe PAST.NName)
  | EvalPure    PAST.NVExpr
  | ReturnBind  PAST.NVExpr
  | DropOneOut
  | MapLookup   WithSem PAST.NVExpr PAST.NVExpr
  | MapInsert   WithSem PAST.NVExpr PAST.NVExpr PAST.NVExpr
  | CoerceCheck WithSem NType NType PAST.NVExpr
  | SelUnion    WithSem PAST.NVExpr Label
  | SelJust     WithSem PAST.NVExpr
  | Guard       PAST.NVExpr


data BranchAction =
    CutBiasAlt State
  | CutLocal
  | CutGlobal
  | FailAction (Maybe PAST.NVExpr) (Maybe PAST.NVExpr)


data Action =
    EpsA
  | IAct WithSem InputAction
  | CAct ControlAction
  | SAct SemanticAction
  | BAct BranchAction


instance Show(InputAction) where
  show (ClssAct _)  = "Match"
  show (IEnd)       = "END"
  show (IOffset)    = "IOffset"
  show (IGetByte)   = "GetByte"
  show (IMatchBytes _) = "MatchBytes"
  show (CurrentStream) = "StreamCurr"
  show (SetStream _)   = "StreamSet"
  show (StreamLen _ _) = "StreamLen"
  show (StreamOff _ _) = "StreamOff"

instance Show(ControlAction) where
  show (BoundSetup _)      = "BoundSetup"
  show (BoundCheckSuccess) = "BoundCheckSuccess"
  show (BoundIncr)         = "BoundIncr"
  show (BoundIsMore)       = "BoundIsMore"
  show (ActivateFrame _) = "ActivateFrame"
  show (DeactivateReady) = "DeactivateReady"
  show (Push n _ dest)     = "Push" ++ "_" ++ (tail $ init $ show n) ++ "_" ++ show dest
  show (Pop dest)          = "Pop" ++ "_" ++ show dest
  show (ForInit _ _ _ _)   = "ForInit"
  show (ForHasMore)        = "ForHasMore"
  show (ForNext)           = "ForNext"
  show (ForEnd)            = "ForEnd"
  show (MapInit _ _)       = "MapInit"
  show (MapHasMore)        = "MapHasMore"
  show (MapNext)           = "MapNext"
  show (MapEnd)            = "MapEnd"

instance Show(SemanticAction) where
  show (EnvFresh)           = "EnvFresh"
  show (ManyFreshList _)    = "ManyFreshList"
  show (ManyAppend  _)      = "ManyAppend"
  show (EnvStore    _)      = "EnvStore"
  show (EvalPure    _)      = "EvalPure" -- ++ (show e)
  show (ReturnBind  _)      = "ReturnBind" -- ++ (show e)
  show (DropOneOut)         = "DropOneOut"
  show (MapLookup   _ _ _)  = "MapLookup"
  show (MapInsert   _ _ _ _) = "MapInsert"
  show (CoerceCheck _ _ _ _) = "CoerceCheck"
  show (SelUnion    _x _y _lbl)  = "SelUnion" -- ++ " " ++ show x ++ " " ++ show y ++ " " ++ show lbl
  show (SelJust     _ _)    = "SelJust"
  show (Guard       _)      = "Guard"


instance Show(BranchAction) where
  show (CutBiasAlt _) = "CutBiasAlt"
  show (CutLocal)     = "CutLocal"
  show (CutGlobal)    = "CutGlobal"
  show (FailAction _ _) = "FailAction"

instance Show(Action) where
  show EpsA             = "eps"
  show (IAct s a)       = show a ++ (if s == YesSem then "_YS" else "_NS")
  show (CAct a)         = show a
  show (SAct a)         = show a
  show (BAct a)         = show a

isClassActOrEnd :: Action -> Bool
isClassActOrEnd act =
  case act of
    IAct _ iact ->
      case iact of
        ClssAct _ -> True
        IEnd -> True
        _ -> False
    _ -> False

isNonClassInputAct :: Action -> Bool
isNonClassInputAct act =
  case act of
    IAct _ iact ->
      case iact of
        ClssAct _ -> False
        _ -> True
    _ -> False

getClassActOrEnd :: Action -> Either PAST.NCExpr InputAction
getClassActOrEnd act =
  case act of
    IAct _ iact ->
      case iact of
        ClssAct ca -> Left ca
        IEnd -> Right iact
        _ -> error "function should be applied on act"
    _ -> error "function should be applied on act"

getMatchBytes :: Action -> Maybe  PAST.NVExpr
getMatchBytes act =
  case act of
    IAct _ iact ->
      case iact of
        IMatchBytes e -> Just e
        _ -> Nothing
    _ -> Nothing

getByteArray :: PAST.NVExpr -> Maybe [Word8]
getByteArray e =
  case e of
    PAST.NByteArray w -> Just (BS.unpack w)
    _ -> Nothing

type Val = Interp.Value

data BeetweenItv =
    CExactly Int
  | CBetween (Maybe Int) (Maybe Int)
  deriving (Show)

data ActivationFrame =
    ListArgs  [Val]
  | ActivatedFrame (Map.Map PAST.NName Val)
  deriving (Show)



data ForFrm = ForFrm
  { forResultName :: PAST.NName
  , forResultValue :: Val
  , forArrElmName :: PAST.NName
  , forArrValue :: Val
  , forSavedOut :: SemanticData
  }

data MapFrm = MapFrm
  { mapResultValue :: Val
  , mapArrElmName :: PAST.NName
  , mapArrValue :: Val
  , mapSavedOut :: SemanticData
  }

data ControlElm =
    ManyFrame (BeetweenItv) Int -- the integer is the current counter
  | ForFrame ForFrm
  | MapFrame MapFrm
  | CallFrame PAST.NName State (ActivationFrame) SemanticData
  --deriving (Eq)

instance Show (ControlElm) where
  show (ManyFrame (CExactly i) k) = "[" ++ show i ++ "] curr:"++ show k
  show (ManyFrame (CBetween i j) k) = "[" ++ show i ++ "," ++ show j ++ "] curr:"++ show k
  show (ForFrame (ForFrm
                 { forResultName = n1
                 , forResultValue = v1
                 , forArrElmName = n2
                 , forArrValue = v2
                 , forSavedOut = _
                 }))
    = "(ForFrame " ++ show n1 ++ ", " ++ show v1 ++ ", " ++ show n2 ++ ", " ++ show v2 ++ ")"
  show (MapFrame (MapFrm
                 { mapResultValue = v1
                 , mapArrElmName = n2
                 , mapArrValue = v2
                 , mapSavedOut = _}))
    = "(MapFrame " ++ show v1 ++ ", " ++ show n2 ++ ", " ++ show v2 ++ ")"
  show (CallFrame name q (ListArgs _) _) = "(CallFrame " ++ show name ++ " " ++ show q ++ " L _)"
  show (CallFrame name q (ActivatedFrame _) _) = "(CallFrame " ++ show name ++ " " ++ show q ++ " A _)"


data SemanticElm =
    SEnvMap (Map.Map PAST.NName Val)
  | SEVal Val
  | SEnd

instance Show (SemanticElm) where
  show (SEnvMap m) = "{" ++ (
    Map.foldrWithKey (\ k a b -> show k ++ ": " ++ show (pp a) ++ "\n" ++ b ) "}\n" m
    )
  show (SEVal v) = show v ++ "\n"
  show (SEnd) = "SEnd\n"

type InputData    = Input
type ControlData  = [ControlElm]
type SemanticData = [SemanticElm]

isEmptyControlData :: ControlData -> Bool
isEmptyControlData [] = True
isEmptyControlData _  = False

showCallStack :: ControlData -> String
showCallStack ctrl =
  case ctrl of
    [] -> "context:"
    x : xs ->
      case x of
        CallFrame name _ _ _ -> showCallStack xs ++ "\n" ++ spacing ++ show name
        _ -> showCallStack xs
  where spacing = "  "


getByte :: Input -> Maybe (Word8,Input)
getByte Input { .. } =
  do (w,bs) <- BS.uncons inputBytes
     let i1 = Input { inputBytes = bs, inputOffset = inputOffset + 1 }
     i1 `seq` pure (w, i1)

-- Lookup in the semantic data first, and then the control
lookupEnvName :: PAST.NName -> ControlData -> SemanticData -> Val
lookupEnvName nname ctrl out =
  {-# SCC lookupEnv #-}
  case lookupSem out ctrl of
    Nothing -> error ("unexpected, missing var from ctrl and out:" ++ show nname)
    Just v  -> v
  where
    lookupSem semOut nextctrl =
      case semOut of
        SEnvMap m : rest ->
          case Map.lookup nname m of
            Nothing -> lookupSem rest nextctrl
            Just v -> Just v
        SEVal _ : rest -> lookupSem rest nextctrl
        SEnd : rest    -> lookupSem rest nextctrl
        [] -> lookupCtrl nextctrl

    lookupCtrl [] = error ("unexpected, missing var:" ++ show nname ++ " from out")
    lookupCtrl (ForFrame (ForFrm
                          { forResultName = n1
                          , forResultValue = v1
                          , forArrElmName = n2
                          , forArrValue = v2
                          , forSavedOut = out1
                          }) : rest) =
      if n1 == nname
      then Just v1
      else if n2 == nname
           then case v2 of
                  Interp.VArray arr -> Just (Vector.head arr)
                  _ -> error "The for iterator should be an array"
           else lookupSem out1 rest
    lookupCtrl (MapFrame (MapFrm
                          { mapResultValue = _
                          , mapArrElmName = n2
                          , mapArrValue = v2
                          , mapSavedOut = out1
                          }) : rest)
      =
      if n2 == nname
      then case v2 of
             Interp.VArray arr ->
               Just (Vector.head arr)
             _ -> error "The for iterator should be an array"
      else lookupSem out1 rest

    lookupCtrl (CallFrame _ _ (ActivatedFrame m) _ : _rest) =
      case Map.lookup nname m of
        Nothing -> Nothing -- The lookup stops at a grammar call
        Just v -> Just v
    lookupCtrl (ManyFrame _ _ : rest) = lookupCtrl rest
    lookupCtrl _ = error "Case not handled"


applyBinop :: BinOp -> Val -> Val -> Val
applyBinop op e1 e2 =
  case op of
    Add ->
      case (e1, e2) of
        (Interp.VUInt p1 v1, Interp.VUInt p2 v2) ->
          if p1 == p2
          then Interp.VUInt p1 (v1 + v2)
          else error "Incompatible precision"
        (Interp.VInteger v1, Interp.VInteger v2) -> Interp.VInteger (v1 + v2)
        _ -> error "Impossible values"
    Sub ->
      case (e1, e2) of
        (Interp.VUInt p1 v1, Interp.VUInt p2 v2) ->
          if p1 == p2
          then Interp.VUInt p1 (v1 - v2)
          else error "Incompatible precision"
        (Interp.VInteger v1, Interp.VInteger v2) -> Interp.VInteger (v1 - v2)
        _ -> error "Impossible values"
    Mul ->
      case (e1, e2) of
        (Interp.VUInt p1 v1, Interp.VUInt p2 v2) ->
          if p1 == p2
          then Interp.VUInt p1 (v1 * v2)
          else error "Incompatible precision"
        (Interp.VInteger v1, Interp.VInteger v2) -> Interp.VInteger (v1 * v2)
        _ -> error "Impossible values"
    Lt ->
      case (e1, e2) of
        (Interp.VUInt p1 v1, Interp.VUInt p2 v2) ->
          if p1 == p2
          then Interp.VBool (v1 < v2)
          else error "Incompatible precision"
        (Interp.VInteger v1, Interp.VInteger v2) -> Interp.VBool (v1 < v2)
        _ -> error "Impossible values"
    Eq ->
      case (e1, e2) of
        (Interp.VUInt p1 v1, Interp.VUInt p2 v2) ->
          if p1 == p2
          then Interp.VBool (v1 == v2)
          else error "Incompatible precision"
        (Interp.VInteger v1, Interp.VInteger v2) -> Interp.VBool (v1 == v2)
        (Interp.VArray arr1, Interp.VArray arr2) ->
          Interp.VBool $ iterateTest arr1 arr2
          where iterateTest array1 array2 =
                  if (Vector.null array1)
                  then if Vector.null array2
                       then True
                       else False
                  else if Vector.null array2
                       then False
                       else
                         let h1 = Vector.head array1
                             t1 = Vector.tail array1
                             h2 = Vector.head array2
                             t2 = Vector.tail array2
                         in
                           case applyBinop Eq h1 h2 of
                             Interp.VBool b -> if not b then False
                                               else iterateTest t1 t2
                             _ -> error "Eq test cannot return something else than a boolean"
        _ -> error $ "Impossible values: " ++ show (e1, e2)
    NotEq ->
      case applyBinop Eq e1 e2 of
        Interp.VBool b -> Interp.VBool (not b)
        _ -> error "Eq test cannot return something else than a boolean"
    Leq ->
      case (e1, e2) of
        (Interp.VUInt p1 v1, Interp.VUInt p2 v2) ->
          if p1 == p2
          then Interp.VBool (v1 <= v2)
          else error "Incompatible precision"
        (Interp.VInteger v1, Interp.VInteger v2) -> Interp.VBool (v1 <= v2)
        _ -> error "Impossible values"
    LShift ->
      case (e1, e2) of
        (Interp.VUInt p1 v1, Interp.VInteger v2) ->
          Interp.VUInt p1 (shiftL v1 (fromIntegral v2))
        (Interp.VInteger v1, Interp.VInteger v2) ->
           Interp.VInteger (shiftL v1 (fromIntegral v2))
        _ -> error ("Impossible values: " ++ show (e1,e2))
    RShift ->
      case (e1, e2) of
        (Interp.VUInt p1 v1, Interp.VInteger v2) ->
          Interp.VUInt p1 (shiftR v1 (fromIntegral v2))
        (Interp.VInteger v1, Interp.VInteger v2) ->
          Interp.VInteger (shiftR v1 (fromIntegral v2))
        _ -> error ("Impossible values: " ++ show (e1,e2))
    BitwiseXor ->
      case (e1, e2) of
        (Interp.VUInt p1 v1, Interp.VUInt p2 v2) ->
          if p1 == p2
          then Interp.VUInt p1 (xor v1 v2)
          else error "Incompatible precision"
        _ -> error ("Impossible values: " ++ show op ++ show (e1,e2))
    BitwiseAnd ->
      case (e1, e2) of
        (Interp.VUInt p1 v1, Interp.VUInt p2 v2) ->
          if p1 == p2
          then Interp.VUInt p1 ((.&.) v1 v2)
          else error "Incompatible precision"
        _ -> error ("Impossible values: " ++ show op ++ show (e1,e2))
    BitwiseOr ->
      case (e1, e2) of
        (Interp.VUInt p1 v1, Interp.VUInt p2 v2) ->
          if p1 == p2
          then Interp.VUInt p1 ((.|.) v1 v2)
          else error "Incompatible precision"
        _ -> error ("Impossible values: " ++ show op ++ show (e1,e2))


    _ -> error ("TODO: " ++ show op)


name2Text :: Name -> Text
name2Text n =
  let x = nameScope n
  in case x of
    Unknown ident -> ident
    Local   ident -> ident
    ModScope _ ident -> ident

name2Text2 :: PAST.NName -> Text
name2Text2 n =
  let x = nameScope (PAST.nName n)
  in case x of
    Unknown ident -> ident
    Local   ident -> ident
    ModScope _ ident -> ident

tODOCONST :: Integer
tODOCONST = 2


coerceVal :: NType -> NType -> Val -> Val
coerceVal ty1 ty2 v =
  case v of
    Interp.VUInt _n x ->
      case ty2 of
        NType (TUInt (NType (TNum m))) -> Interp.VUInt (fromIntegral m) x
        NType (TInteger) -> Interp.VInteger x
        _ -> error $ "TODO type:" ++ show (ty1,ty2,v)
    Interp.VInteger _ ->
      case (ty1, ty2) of
        (NType (TInteger), NType (TInteger)) -> v
        _ -> error $ "TODO type: " ++ show (ty1,ty2,v)
    _ -> error $ "TODO vallue: " ++ show (ty1,ty2,v)


isSimpleVExpr :: PAST.NVExpr -> Bool
isSimpleVExpr e =
  case e of
    PAST.NCoerce _ _ _ -> False
    PAST.NNumber _ _ -> True
    PAST.NBool _ -> False
    PAST.NNothing _ty -> False
    PAST.NByte _ -> True
    PAST.NStruct _lst _ -> False
    PAST.NByteArray _ -> False
    PAST.NMapEmpty _ty -> False
    PAST.NIn _lbl _e1 _ty -> False
    PAST.NBinOp _binop _e1 _e2 -> False
    PAST.NUniOp _uniop _e1 -> False
    PAST.NSelStruct _e1 _n _ty -> False
    PAST.NIf _e1 _e2 _e3 -> False
    PAST.NVCall _fname _lst -> False
    PAST.NVFor _n1 _e1 _n2 _e2 _e3 -> False
    PAST.NVar _nname -> False
    PAST.NArray _lste _ty -> False
    PAST.NUnit -> False
    x -> error ("TODO: "++ show x)

defaultValue :: Val
defaultValue = Interp.VStruct []

evalVExpr :: PAST.GblFuns -> PAST.NVExpr -> ControlData -> SemanticData -> Val
evalVExpr gbl expr ctrl out =
  let eval env e =
        case e of
          PAST.NCoerce ty1 ty2 e1 -> coerceVal ty1 ty2 (eval env e1)
          PAST.NNumber n t ->
            case t of
              NType (TUInt (NType (TNum m))) -> Interp.VUInt (fromIntegral m) n
              NType (TInteger) -> Interp.VInteger n
              _ -> error "TODO: more type for integer"
          PAST.NBool b -> Interp.VBool b
          PAST.NNothing _ty ->
            Interp.VMaybe (Nothing)
          PAST.NJust e1 ->
            let ve1 = eval env e1
            in Interp.VMaybe (Just ve1)
          PAST.NByte w -> Interp.VUInt 8 (fromIntegral w)
          PAST.NStruct lst _ ->
            Interp.VStruct (map (\ (lbl, v) -> (lbl, eval env v)) lst)
          PAST.NByteArray bs ->
            Interp.VArray (Vector.fromList (map (\w -> Interp.VUInt 8 (fromIntegral w)) (BS.unpack bs)))
          PAST.NMapEmpty _ty -> Interp.VMap (Map.empty)
          PAST.NIn lbl e1 _ty ->
            let ve1 = eval env e1 in
            Interp.VUnionElem lbl ve1
          PAST.NBinOp binop e1 e2 ->
            let ve1 = eval env e1
                ve2 = eval env e2
            in
              applyBinop binop ve1 ve2
          PAST.NUniOp uniop e1 ->
            let ve1 = eval env e1
            in case (ve1, uniop) of
              (Interp.VBool b, Not) -> Interp.VBool (not b)
              (Interp.VInteger i, Neg) -> Interp.VInteger (negate i)
              (Interp.VArray v, Concat) ->
                Interp.VArray
                (Vector.fromList (Vector.foldr (\ a b ->
                          case a of
                            Interp.VArray av -> Vector.toList av ++ b
                            _ -> error "element in Array concatenation is not an Array")
                  [] v
                ))
              x -> error ("not Integer type" ++ show x ++ show uniop)
          PAST.NSelStruct e1 n _ty ->
            let ve1 = eval env e1
            in case ve1 of
              Interp.VStruct lst -> snd $ head $ filter (\ (a,_) -> a == n) lst
              _ -> error "SEl must be on Struct"
          PAST.NIf e1 e2 e3 ->
            case eval env e1 of
              Interp.VBool True -> eval env e2
              Interp.VBool False -> eval env e3
              _ -> error "cond must be bool"
          PAST.NVCall fname lst ->
            let elst = map (\ expre -> eval env expre) lst
                fname1 = PAST.nName fname
                argsAndBody = Map.lookup fname1 gbl
                (env1, body) = case argsAndBody of
                  Nothing -> error ("Fun '" ++ show (name2Text (PAST.nName fname)) ++ "' not found!")
                  Just (args, bdy) ->
                    let lstArgsValues = zip (map (name2Text . PAST.nName) args) elst in
                    (lstArgsValues, bdy)
            in case body of
                 PAST.ValueExpression bdy -> eval (Just env1) bdy
                 _ -> error "Unexpected body type, not a ValueExpression"
          PAST.NVFor n1 e1 n2 e2 e3 ->
            case eval env e2 of
              Interp.VArray vect ->
                Vector.foldl f (eval env e1) vect
                where f b a =
                        let localenv = concatLocalEnv (Just ([(name2Text (PAST.nName n1), b), (name2Text (PAST.nName n2), a)])) env
                        in eval localenv e3
              _ -> error "Unexpected type in For"
          PAST.NVar nname ->
            case env of
              Just env1 -> -- inside a function call
                case lookupLocalEnv (name2Text (PAST.nName nname)) env1 of
                  Nothing -> error ("variable not bound by function: " ++ (show nname))
                  Just v -> v
              Nothing -> -- not inside a function call
                lookupEnvName nname ctrl out
          PAST.NArray lste _ty ->
            let lev = map (\ ex -> eval env ex) lste
            in Interp.VArray (Vector.fromList lev)
          PAST.NUnit -> defaultValue
          x -> error ("TODO: "++ show x)

  in eval Nothing expr
  where concatLocalEnv env1 env2 =
          case (env1, env2) of
            (Just env11, Just env22) -> Just (env11 ++ env22)
            (Just _, Nothing) -> env1
            (Nothing, Just _) -> env2
            (Nothing, Nothing) -> Nothing
        lookupLocalEnv n1 env =
          foldl (\b (n2, v) -> case b of
                                 Nothing -> if n1 == n2 then Just v else Nothing
                                 Just v1 -> Just v1
                ) Nothing env

evalNoFunCall:: PAST.NVExpr -> ControlData -> SemanticData -> Val
evalNoFunCall e ctrl out = evalVExpr (Map.empty) e ctrl out

valToInt :: Val -> Int
valToInt v =
  case v of
    Interp.VUInt 8 i -> toEnum (fromIntegral i ::Int)
    Interp.VInteger i -> fromIntegral i :: Int
    _ -> error "valToInt pb"

evalCExpr :: PAST.GblFuns -> PAST.NCExpr -> Word8 -> ControlData -> SemanticData -> Maybe SemanticElm
evalCExpr gbl expr x ctrl out =
  case expr of
    PAST.NSetSingle e     ->
      let v = evalNoFunCall e ctrl out in
      let j =  valToInt v in
        if  x == toEnum j
        then let o = SEVal v
             in Just o
        else Nothing
    PAST.NSetComplement e     ->
      case evalCExpr gbl e x ctrl out of
        Nothing -> Just (SEVal (Interp.VUInt 8 (fromIntegral x)))
        Just _ -> Nothing
    PAST.NSetUnion lst     ->
      let go l =
            case l of
              []   -> Nothing
              e:es ->
                let v = evalCExpr gbl e x ctrl out in
                  case v of
                    Nothing -> go es
                    Just o -> Just o
      in go lst
    PAST.NSetOneOf bs     ->
      if elem x (BS.unpack bs)
      then Just (SEVal (Interp.VUInt 8 (fromIntegral x)))
      else Nothing
    PAST.NSetRange e1 e2  ->
      let v1 = evalNoFunCall e1 ctrl out
          v2 = evalNoFunCall e2 ctrl out
      in let j1 = valToInt v1
             j2 = valToInt v2
         in if toEnum j1 <= x && x <= toEnum j2
            then let o = SEVal (Interp.VUInt 8 (fromIntegral x))
                 in Just o
            else Nothing
    PAST.NCCall cname _lst -> -- this code is a simplification of PAST.NVCall
      let cname1 = PAST.nName cname
          argsAndBody = Map.lookup cname1 gbl
          body = case argsAndBody of
                   Nothing -> error ("Class Fun '" ++ show (name2Text (PAST.nName cname)) ++ "' not found!")
                   Just (_args, bdy) -> bdy
      in case body of
           PAST.ClassExpression bdy -> evalCExpr gbl bdy x ctrl out
           _ -> error "Unexpected body type not a ClassExpression"
    _ -> error ("TODO: " ++ (show expr))

-- copy from rts-hs/src/RTS/ParserAPI.hs
-- | Limit the input to the given number of bytes.
-- Fails if there aren't enough bytes.
limitLen :: Integer -> Input -> Maybe Input
limitLen n' i =
  do n <- Just (fromIntegral n')
     let bs = inputBytes i
     guard (0 <= n && n <= BS.length bs)
     pure i { inputBytes = BS.take n bs }


-- copy from rts-hs/src/RTS/ParserAPI.hs
-- Fails if we don't have enough bytes, although it is ok to
-- get to the very end of the input.
advanceBy :: Integer -> Input -> Maybe Input
advanceBy n' i =
  do n <- Just (fromIntegral n')
     let bs = inputBytes i
     guard (0 <= n && n <= BS.length bs)
     pure Input { inputBytes  = BS.drop n bs
                , inputOffset = inputOffset i + n
                }


applyInputAction :: PAST.GblFuns -> (InputData, ControlData, SemanticData) -> WithSem -> InputAction -> Maybe (InputData, SemanticData)
applyInputAction gbl (inp, ctrl, out) s act =
  case act of
    ClssAct e ->
      case getByte inp of
        Nothing -> Nothing
        Just (x, xs) ->
          let b = evalCExpr gbl e x ctrl out
          in case b of
               Nothing -> Nothing
               Just o -> resultWithSem s xs o
    IEnd ->
      case getByte inp of
        Nothing -> resultWithSem s inp SEnd
        _ -> Nothing
    IOffset -> Just (inp, SEVal (Interp.VInteger (fromIntegral (inputOffset inp)))  : out)
    IGetByte ->
      case getByte inp of
        Nothing -> Nothing
        Just (x, xs) -> resultWithSem s xs (SEVal (Interp.VUInt 8 (fromIntegral x)))
    IMatchBytes e1     ->
      let ev1 = evalVExpr gbl e1 ctrl out
      in case ev1 of
           Interp.VArray vec ->
             let len = fromIntegral (Vector.length vec)
             in
             case limitLen len inp of
               Nothing -> Nothing
               Just inp1 ->
                 if BS.unpack (inputBytes inp1) == map (\v -> toEnum (valToInt v)) (Vector.toList vec)
                 then let i = fromJust $ advanceBy len inp
                          o = SEVal (Interp.VArray vec)
                      in resultWithSem s i o
                 else Nothing
           _ -> error ("unexpected match bytes: "++ show e1)
    CurrentStream ->
      Just (inp, SEVal (Interp.VStream inp) : out)
    SetStream e1 ->
      let ev1 = evalVExpr gbl e1 ctrl out
      in case ev1 of
           Interp.VStream i1 -> Just (i1, SEVal (defaultValue) {- technically just for an invariant at the EnvStore handling -} : out)
           _ -> error "Not an input stream at this value"
    StreamLen e1 e2 ->
      let ev1 = evalVExpr gbl e1 ctrl out
          ev2 = evalVExpr gbl e2 ctrl out
      in case ev1 of
           Interp.VInteger n ->
             case ev2 of
               Interp.VStream i1 ->
                 case limitLen n i1 of
                   Nothing -> error "stream value too short for len"
                   Just i2 -> Just (inp, SEVal (Interp.VStream i2) : out)
               _ -> error "Not an input stream"
           _ -> error "Not an integer for Taking"
    StreamOff e1 e2 ->
      let ev1 = evalVExpr gbl e1 ctrl out
          ev2 = evalVExpr gbl e2 ctrl out
      in case ev1 of
        Interp.VInteger n ->
          case ev2 of
            Interp.VStream i1 ->
              case advanceBy n i1 of
                Nothing -> error "stream too short for advance"
                Just i2 -> Just (inp, SEVal (Interp.VStream i2) : out)
            _ -> error "Not an input stream"
        _ -> error "Not an integer for Taking"
  where
    resultWithSem YesSem i o = Just (i, o : out)
    resultWithSem NoSem  i _ = Just (i, SEVal (defaultValue) : out)



initSemanticData :: SemanticData
initSemanticData = []

applyControlAction :: PAST.GblFuns -> (ControlData, SemanticData) -> ControlAction -> Maybe (ControlData, SemanticData)
applyControlAction gbl (ctrl, out) act =
  case act of
    BoundSetup bound ->
      case bound of
        Exactly v ->
          let ev = evalVExpr gbl v ctrl out
              i = valToInt ev
          in Just (ManyFrame (CExactly i) 0 : ctrl, out)
        Between v1 v2 ->
          let ev1 = fmap (\v -> valToInt (evalVExpr gbl v ctrl out)) v1
              ev2 = fmap (\v -> valToInt (evalVExpr gbl v ctrl out)) v2
          in  Just (ManyFrame (CBetween ev1 ev2) 0 : ctrl, out)
    BoundCheckSuccess ->
      case ctrl of
        [] -> error "Unexpected ctrl stack"
        ManyFrame (CExactly i) cnt : rest ->
          if i == cnt
          then Just (rest, out)
          else if i < 0
               then Just (rest, out) -- case aligned with DaeDaLus interp. `Nothing` could be another option
               else Nothing
        ManyFrame (CBetween i j) cnt : rest ->
          case (i, j) of
            (Nothing, Nothing) -> Just (rest, out)
            (Nothing, Just jj) -> if jj >= cnt then Just (rest, out) else Nothing
            (Just ii, Nothing) -> if ii <= cnt then Just (rest, out) else Nothing
            (Just ii, Just jj) ->
              if ii <= cnt && jj >= cnt then Just (rest, out) else Nothing
        _ -> error "Unexpected ctrl stack top element"
    BoundIsMore ->
      case ctrl of
        [] -> error "Unexpected ctrl stack"
        ManyFrame (CExactly i) cnt : _ ->
          if i > cnt then Just (ctrl, out) else Nothing
        ManyFrame (CBetween _ j) cnt : _ ->
          case j of
            Nothing -> Just (ctrl, out)
            Just jj -> if jj > cnt then Just (ctrl, out) else Nothing
        _ -> error "Unexpected ctrl stack top element"
    BoundIncr ->
      case ctrl of
        [] -> error "Unexpected ctrl stack"
        ManyFrame bound cnt : rest -> Just (ManyFrame bound (cnt+1) : rest, out)
        _ -> error "Unexpected ctrl stack top element"
    Push name le q ->
      let evle = map (\ e -> evalVExpr gbl e ctrl out) le
      in Just (CallFrame name q (ListArgs evle) out : ctrl, [])
    Pop q ->
      case ctrl of
        [] -> error "Unexpected empty ctrl stack"
        CallFrame _ q' _ savedOut : rest -> if q == q' then Just (rest, head out : savedOut) else Nothing
        _ -> error "Unexpected ctrl stack elm"
    ForInit n1 e1 n2 e2 ->
      let ev1 = evalVExpr gbl e1 ctrl out
          ev2 = evalVExpr gbl e2 ctrl out
          forfrm = ForFrm
            { forResultName = n1
            , forResultValue = ev1
            , forArrElmName = n2
            , forArrValue = ev2
            , forSavedOut = out
            }
      in Just (ForFrame forfrm : ctrl, initSemanticData)
    ForHasMore ->
      case ctrl of
        [] -> error "Unexpected empty ctrl stack"
        ForFrame forfrm : _ ->
          case forArrValue forfrm of
            Interp.VArray arr ->
              if Vector.null arr
              then Nothing
              else Just (ctrl, out)
            _ -> error "Unexpected value"
        _ -> error "Unexpected non ForFrame"
    ForNext ->
      case ctrl of
        [] -> error "Unexpected empty ctrl stack"
        ForFrame (
          ForFrm
          { forResultName = n1
          , forResultValue = _ev1
          , forArrElmName = n2
          , forArrValue = ev2
          , forSavedOut = savedOut
          }) : rest ->
          --if length out /= (1::Int)
          --then error "sdfsdf"
          --else
          case ev2 of
            Interp.VArray arr ->
              if Vector.null arr
              then Nothing
              else case head out of
                     SEVal b ->
                       let forfrm = ForFrm
                             { forResultName = n1
                             , forResultValue = b
                             , forArrElmName = n2
                             , forArrValue = Interp.VArray (Vector.tail arr)
                             , forSavedOut = savedOut
                             }
                       in
                       Just (ForFrame forfrm : rest, initSemanticData)
                     _ -> error "the semantic value should be a Val"
            _ -> error "Unexpected For not an array"
        _ -> error "Unexpected ctrl stack"
    ForEnd ->
      case ctrl of
        [] -> error "Unexpected empty ctrl stack"
        ForFrame (
          ForFrm
          { forResultName = _n1
          , forResultValue = ev1
          , forArrElmName = _n2
          , forArrValue = ev2
          , forSavedOut = savedOut
          }) : rest ->
          case ev2 of
            Interp.VArray arr ->
              if Vector.null arr
              then Just (rest, (SEVal ev1) : savedOut)
              else Nothing
            _ -> error "Unexpected For not an array"
        _ ->  error "Unexpected ctrl stack"

    MapInit n1 e1 ->
      let ev1 = evalVExpr gbl e1 ctrl out
          mapfrm = MapFrm
            { mapResultValue = (Interp.VArray Vector.empty)
            , mapArrElmName = n1
            , mapArrValue = ev1
            , mapSavedOut = out
            }
      in Just (MapFrame mapfrm : ctrl, initSemanticData)
    MapHasMore ->
      case ctrl of
        [] -> error "Unexpected empty ctrl stack"
        MapFrame mapfrm : _ ->
          case mapArrValue mapfrm of
            Interp.VArray arr ->
              if Vector.null arr
              then Nothing
              else Just (ctrl, out)
            _ -> error "Unexpected value"
        _ -> error "Unexpected non MapFrame"
    MapNext ->
      case ctrl of
        [] -> error "Unexpected empty ctrl stack"
        MapFrame (MapFrm
            { mapResultValue = ev1
            , mapArrElmName = n2
            , mapArrValue = ev2
            , mapSavedOut = savedOut
            }) : rest ->
          --if length out /= (1::Int)
          --then error "sdfsdf"
          --else
          case ev2 of
            Interp.VArray arr ->
              -- trace ("SIZE of arr: " ++ show (length arr)) $
              if Vector.null arr
              then Nothing
              else case head out of
                     SEVal b ->
                       case ev1 of
                         Interp.VArray v ->
                           let newAccArr = Interp.VArray (Vector.snoc v b)
                               restArr = Interp.VArray (Vector.tail arr)
                               mapfrm = MapFrm
                                 { mapResultValue = newAccArr
                                 , mapArrElmName = n2
                                 , mapArrValue = restArr
                                 , mapSavedOut = savedOut
                                 }
                           in Just (MapFrame mapfrm : rest, initSemanticData)
                         _ -> error "the array accumulator should be an array"
                     _ -> error "the semantic value should be a Val"
            _ -> error "Unexpected For not an array"
        _ -> error "Unexpected ctrl stack"
    MapEnd ->
      case ctrl of
        [] -> error "Unexpected empty ctrl stack"
        MapFrame (MapFrm
            { mapResultValue = ev1
            , mapArrElmName = _n2
            , mapArrValue = ev2
            , mapSavedOut = savedOut
            }) : rest ->
          case ev2 of
            Interp.VArray arr ->
              if Vector.null arr
              then Just (rest, (SEVal ev1) : savedOut)
              else Nothing
            _ -> error "Unexpected For not an array"
        _ ->  error "Unexpected ctrl stack"

    ActivateFrame ln ->
      case ctrl of
        CallFrame rname q (ListArgs lvs) savedFrame : ctrls ->
          let zipped =
                if length ln == length lvs
                then zip lvs ln
                else error "activate"

              activatedFrame = ActivatedFrame (
                foldr (\ (val, name) set -> (Map.insert name val set)) Map.empty zipped)
              in
              Just (CallFrame rname q activatedFrame savedFrame : ctrls, out)
        _ -> error "unexpected ctrl stack, not a CallFrame ListArgs"
    DeactivateReady -> (
      case ctrl of
        CallFrame _rname _q (ActivatedFrame _) _savedFrame : _ctrls -> Just (ctrl, out)
        _ -> error "unexpected out"
      )


applySemanticAction :: PAST.GblFuns -> (ControlData, SemanticData) -> SemanticAction -> Maybe SemanticData
applySemanticAction gbl (ctrl, out) act =
  case act of
    EnvFresh -> Just (SEnvMap Map.empty : out)
    ManyFreshList s ->
      case s of
        YesSem -> Just (SEVal (Interp.VArray Vector.empty) : out)
        NoSem  -> Just (SEVal (defaultValue) : out)
    ManyAppend YesSem ->
      case out of
        x : (SEVal (Interp.VArray y)) : z  -> (
          case x of
            SEVal v -> Just (SEVal (Interp.VArray (Vector.snoc y v)) : z)
            _ -> error "unexpected out, cannot proceed"
          )
        _ -> error "unexpected out, cannot proceed"
    ManyAppend NoSem ->
      case out of
        _ : e@((SEVal (Interp.VStruct [])) : _)  -> Just e
        _ -> error "unexpected out, cannot proceed"
    EnvStore mname -> (
      case out of
        x : rest@((SEnvMap y) : z)  -> (
          case x of
            SEnvMap _ -> error "unexpected out, cannot proceed"
            SEVal v ->
              case mname of
                Nothing -> Just rest
                Just name -> Just (SEnvMap (Map.insert name v y) : z)
            SEnd -> Just rest
          )
        _ -> error ("unexpected out, cannot proceed: " ++ show mname ++ "\nOUT:\n" ++ show out ++ "\nCTRL:\n" ++ show ctrl)
      )
    EvalPure e -> Just (SEVal (evalVExpr gbl e ctrl out) : out)
    ReturnBind e -> Just (SEVal (evalVExpr gbl e ctrl out) : tail out)
    DropOneOut ->
      case out of
        [] -> error "Should not Happen: drop on empty sem stack"
        _ : os -> Just os
    MapLookup _ e1 e2 ->
      let ev1 = evalVExpr gbl e1 ctrl out
          ev2 = evalVExpr gbl e2 ctrl out
      in case ev2 of
           Interp.VMap m -> case Map.lookup ev1 m of
                             Nothing -> Nothing
                             Just v -> Just (SEVal v : out)
           _ -> error "Lookup is not applied to value of type map"
    MapInsert _ e1 e2 e3 ->
      let ev1 = evalVExpr gbl e1 ctrl out
          ev2 = evalVExpr gbl e2 ctrl out
          ev3 = evalVExpr gbl e3 ctrl out
      in case ev3 of
           Interp.VMap m -> Just (SEVal (Interp.VMap (Map.insert ev1 ev2 m)) : out)
           _ -> error "Lookup is not applied to value of type map"
    CoerceCheck _ _t1 _t2 _e1 ->
      -- TODO: incomplete
      Just out
    SelUnion _ e1 lbl ->
      let ev1 = evalVExpr gbl e1 ctrl out
      in case ev1 of
           Interp.VUnionElem lbl1 e2 ->
             if lbl1 == lbl
             then Just (SEVal e2 : out)
             else Nothing
           _ -> error "Selection not a union"

    SelJust _ e1 ->
      let ev1 = evalVExpr gbl e1 ctrl out
      in case ev1 of
           Interp.VMaybe Nothing -> Nothing
           Interp.VMaybe (Just v1) -> Just ((SEVal v1) : out)
           _ -> error "Semantic output not a map"

    Guard e1 ->
      let ev1 = evalVExpr gbl e1 ctrl out
      in case ev1 of
           Interp.VBool b -> if b then Just (SEVal (defaultValue) : out) else Nothing
           _ -> error "Guard must evaluate to a boolean"


applyAction :: PAST.GblFuns -> (InputData, ControlData, SemanticData) -> Action ->
  Maybe (InputData, ControlData, SemanticData)
applyAction gbl (inp, ctrl, out) act =
  case act of
    EpsA       -> Just (inp, ctrl, out)
    IAct s a   -> case applyInputAction gbl (inp, ctrl, out) s a of
                    Nothing -> Nothing
                    Just (inp1, out1) -> Just (inp1, ctrl, out1)
    CAct a     -> case applyControlAction gbl (ctrl, out) a of
                    Nothing -> Nothing
                    Just (ctrl1, out1) -> Just (inp, ctrl1, out1)
    SAct a     -> case applySemanticAction gbl (ctrl, out) a of
                    Nothing -> Nothing
                    Just out1 -> Just (inp, ctrl, out1)
    BAct _     -> error "This case should not be handled in applyAction"
