{-# Language GADTs, DataKinds, ExistentialQuantification, RecordWildCards #-}

module Daedalus.ParserGen.Action
  ( State
  , InputAction(..)
  , SemanticAction(..)
  , BranchAction(..)
  , SemanticElm(..)
  , ControlAction(..)
  , Action(..)
  , InputData
  , ControlData
  , SemanticData
  , isClassActOrEnd
  , isInputAction
  , isActivateFrameAction
  , isUnhandledInputAction
  , isUnhandledAction
  , isPushAction
  , isBoundSetup
  , getMatchBytes
  , getClassActOrEnd
  , isEmptyControlData
  , showCallStackDebug
  , showSemanticData
  , showCallStack
  , callCExpr
  , evalNoFunCall
  , isSimpleVExpr
  , defaultValue
  , evalLiteral
  , valToInt
  , applyAction
  )
where

-- import Debug.Trace
--
import GHC.Float(double2Float)

import qualified Data.Map as Map
import Data.Word
import qualified Data.Vector as Vector
import Data.Maybe (fromJust)
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NonEmpty


import Daedalus.PP hiding (empty)
import qualified Daedalus.Value as Interp
import qualified Daedalus.Interp as Interp

import Daedalus.Type.AST
import RTS.Numeric(intToSize)
import RTS.Input(Input(..))
import qualified RTS.Input as Input

import Daedalus.ParserGen.AST (CorV(..), GblFuns, NVExpr, NCExpr, showNCExpr, showName)

type State = Int


data InputAction =
    ClssAct WithSem NCExpr
  | IEnd
  | IOffset
  | IGetByte WithSem
  | IMatchBytes WithSem NVExpr
  | GetStream
  | SetStream NVExpr
  | StreamTake WithSem NVExpr NVExpr
  | StreamDrop WithSem NVExpr NVExpr


data ControlAction =
    BoundSetup (ManyBounds NVExpr)
  | BoundCheckMore
  | BoundCheckSuccess
  | BoundIncr
  | ActivateFrame [Name]
  | DeactivateReady
  | Push Name [NVExpr] State
  | Pop
  | ForInit Name NVExpr Name NVExpr
  | ForHasMore
  | ForNext
  | ForEnd
  | MapInit Name NVExpr
  | MapHasMore
  | MapNext
  | MapEnd
  | CaseCall NVExpr
  | CaseTry (Maybe [TCPat])
  | CaseEnd


data SemanticAction =
    EnvFresh
  | ManyFreshList WithSem
  | ManyAppend    WithSem
  | ManyReturn
  | EnvStore    (Maybe Name)
  | EvalPure    NVExpr
  | ReturnBind  NVExpr
  | ReturnLast
  | DropOneOut
  | MapLookup   WithSem NVExpr NVExpr
  | MapInsert   WithSem NVExpr NVExpr NVExpr
  | CoerceCheck WithSem Type Type NVExpr
  | Guard       NVExpr


data BranchAction =
    CutBiasAlt State
  | CutLocal
  | CutGlobal
  | FailAction (Maybe NVExpr)


data Action =
    EpsA
  | IAct InputAction
  | CAct ControlAction
  | SAct SemanticAction
  | BAct BranchAction


semToString :: WithSem -> String
semToString YesSem = "S"
semToString NoSem = ""


instance Show(InputAction) where
  show (ClssAct s c)  = semToString s ++ "Match-" ++ showNCExpr c
  show (IEnd)         = "END"
  show (IOffset)      = "IOffset"
  show (IGetByte s)   = semToString s ++ "GetByte"
  show (IMatchBytes s e) = semToString s ++ "MatchBytes " ++ show (pp $ texprValue e)
  show (GetStream)       = "GetStream"
  show (SetStream _)     = "SetStream"
  show (StreamTake s _ _) = semToString s ++ "StreamTake"
  show (StreamDrop s _ _) = semToString s ++ "StreamDrop"

instance Show(ControlAction) where
  show (BoundSetup _)      = "BoundSetup"
  show (BoundCheckMore)    = "BoundCheckMore"
  show (BoundCheckSuccess) = "BoundCheckSuccess"
  show (BoundIncr)         = "BoundIncr"
  show (ActivateFrame _) = "ActivateFrame"
  show (DeactivateReady) = "DeactivateReady"
  show (Push n _ dest)     = "Push" ++ "_" ++ (tail $ init $ showName n) ++ "_" ++ show dest
  show (Pop)               = "Pop"
  show (ForInit _ _ _ _)   = "ForInit"
  show (ForHasMore)        = "ForHasMore"
  show (ForNext)           = "ForNext"
  show (ForEnd)            = "ForEnd"
  show (MapInit _ _)       = "MapInit"
  show (MapHasMore)        = "MapHasMore"
  show (MapNext)           = "MapNext"
  show (MapEnd)            = "MapEnd"
  show (CaseCall _)        = "CaseCall"
  show (CaseTry _)         = "CaseTry"
  show (CaseEnd)           = "CaseEnd"


instance Show(SemanticAction) where
  show (EnvFresh)           = "EnvFresh"
  show (ManyFreshList _)    = "ManyFreshList"
  show (ManyAppend  _)      = "ManyAppend"
  show (ManyReturn)         = "ManyReturn"
  show (EnvStore    _)      = "EnvStore"
  show (EvalPure    _)      = "EvalPure" -- ++ (show e)
  show (ReturnBind  _)      = "ReturnBind" -- ++ (show e)
  show (ReturnLast)         = "ReturnLast"
  show (DropOneOut)         = "DropOneOut"
  show (MapLookup   _ _ _)  = "MapLookup"
  show (MapInsert   _ _ _ _) = "MapInsert"
  show (CoerceCheck _ _ _ _) = "CoerceCheck"
  show (Guard       _)      = "Guard"


instance Show(BranchAction) where
  show (CutBiasAlt _) = "CutBiasAlt"
  show (CutLocal)     = "CutLocal"
  show (CutGlobal)    = "CutGlobal"
  show (FailAction _) = "FailAction"

instance Show(Action) where
  show EpsA             = ""
  show (IAct a)         = show a
  show (CAct a)         = show a
  show (SAct a)         = show a
  show (BAct a)         = show a

isClassActOrEnd :: Action -> Bool
isClassActOrEnd act =
  case act of
    IAct iact ->
      case iact of
        ClssAct _ _ -> True
        IGetByte _ -> True
        IEnd -> True
        _ -> False
    _ -> False

isInputAction :: Action -> Bool
isInputAction act =
  case act of
    IAct iact ->
      case iact of
        ClssAct {} -> True
        IEnd -> True
        _ -> False
    _ -> False

isActivateFrameAction :: Action -> Bool
isActivateFrameAction act =
  case act of
    CAct (ActivateFrame {}) -> True
    _ -> False

isUnhandledInputAction :: Action -> Bool
isUnhandledInputAction act =
  case act of
    IAct iact ->
      case iact of
        ClssAct _ _ -> False
        GetStream -> False
        SetStream _ -> False
        StreamTake {} -> False
        StreamDrop {} -> False
        _ -> -- trace (show iact) $
             True
    _ -> False

isUnhandledAction :: Action -> Bool
isUnhandledAction act =
  case act of
    CAct cact ->
      case cact of
        ForInit {} -> True
        ForHasMore -> True
        ForNext -> True
        ForEnd -> True
        MapInit {} -> True
        MapHasMore -> True
        MapNext -> True
        MapEnd -> True
        CaseCall _ -> True
        CaseTry _ -> True
        CaseEnd -> True
        _ -> False
    SAct sact ->
      case sact of
        MapInsert {} -> True
        Guard _ -> True
        -- CoerceCheck {} -> True
        _ -> False
    _ -> False

isPushAction :: Action -> Bool
isPushAction act =
  case act of
    CAct (Push _ _ _) -> True
    _ -> False

isBoundSetup :: Action -> Bool
isBoundSetup act =
  case act of
    CAct (BoundSetup _) -> True
    _ -> False

getClassActOrEnd :: Action -> Either (Either NCExpr InputAction) InputAction
getClassActOrEnd act =
  case act of
    IAct iact ->
      case iact of
        ClssAct _ ca -> Left $ Left ca
        IGetByte _s -> Left $ Right iact
        IEnd -> Right iact
        _ -> error "function should be applied on act"
    _ -> error "function should be applied on act"

getMatchBytes :: Action -> Maybe NVExpr
getMatchBytes act =
  case act of
    IAct iact ->
      case iact of
        IMatchBytes _ e -> Just e
        _ -> Nothing
    _ -> Nothing


type Val = Interp.Value


data BetweenItv =
    CExactly {-# UNPACK #-} !Int
  | CBetween !(Maybe Int) !(Maybe Int)
  deriving (Show)

-- TODO: rename ListArgs to PreActivated
data ActivationFrame =
    ListArgs ![Val]
  | ActivatedFrame !(Map.Map Name Val)
  deriving (Show)



data ForFrm = ForFrm
  { forResultName :: Name
  , forResultValue :: Val
  , forArrElmName :: Name
  , forArrValue :: Val
  , forSavedOut :: SemanticData
  }

data MapFrm = MapFrm
  { mapResultValue :: Val
  , mapArrElmName :: Name
  , mapArrValue :: Val
  , mapSavedOut :: SemanticData
  }

data CaseFrm =
    CaseOn
    { caseOn :: Val
    , caseSavedOut :: SemanticData
    }
  | CaseBound
    { caseBound :: !(Map.Map Name Val)
    , caseSavedOut :: SemanticData
    }
  deriving (Show)

data ControlElm =
    ManyFrame !(BetweenItv) {-# UNPACK #-} !Int -- the integer is the current counter
  | ForFrame  !ForFrm
  | MapFrame  !MapFrm
  | CaseFrame !CaseFrm
  | CallFrame !Name {-# UNPACK #-} !State !(ActivationFrame) !SemanticData
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
  show (CaseFrame (CaseOn v _)) = "(CaseFrameOn " ++ show v ++ ")"
  show (CaseFrame (CaseBound m _)) = "(CaseFrameBound " ++ show m ++ ")"


data SemanticElm =
    SEnvMap !(Map.Map Name Val)
  | SEVal   !Val
  | SManyVal ![Val]
  | SEnd

instance Show (SemanticElm) where
  show (SEnvMap m) = "{" ++ (
    Map.foldrWithKey (\ k a b -> show k ++ ": " ++ show (pp a) ++ "\n" ++ b ) "}\n" m
    )
  show (SEVal v) = show v ++ "\n"
  show (SManyVal lst) = show lst ++ "\n"
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
        CallFrame name _ _ _ ->
          showCallStack xs ++ "\n"
          ++ spacing ++ showName name
        _ -> showCallStack xs
  where spacing = "  "

showCallStackDebug :: ControlData -> String
showCallStackDebug ctrl =
  case ctrl of
    [] -> "context:"
    x : xs ->
      case x of
        CallFrame name _ _ semData ->
          showCallStackDebug xs ++ "\n"
          ++ (concat $ map (\ s -> s ++ "\n") $ showSemanticData semData)
          ++ spacing ++ showName name
        _ -> showCallStackDebug xs
  where spacing = "  "


showSemanticData :: SemanticData -> [String]
showSemanticData sem =
  case sem of
    [] -> []
    x : xs ->
      showSemanticData xs ++
      case x of
        SEnvMap m ->
          let showm = Map.foldrWithKey (\ k _v acc -> (spacing ++ showName k) : acc) [] m
          in showm
        SManyVal lst ->
          let showm = map (\ _ -> spacing ++ "*") lst
          in showm
        _ -> []
  where
    spacing = "    - "



getByte :: Input -> Maybe (Word8,Input)
getByte = Input.inputByte

-- Lookup in the semantic data first, and then the control
lookupEnvName :: Name -> ControlData -> SemanticData -> Val
lookupEnvName nname ctrl out =
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
        SManyVal _ : rest -> lookupSem rest nextctrl
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

    lookupCtrl (CaseFrame (CaseBound
                          { caseBound = m
                          , caseSavedOut = out1
                          }) : rest)
      =
      case Map.lookup nname m of
        Nothing -> lookupSem out1 rest
        Just v -> Just v

    lookupCtrl (CaseFrame (CaseOn {}) : _) =
      error "broken invariant on ctrl, cannot be this case"

    lookupCtrl (CallFrame _ _ (ActivatedFrame m) _ : _rest) =
      case Map.lookup nname m of
        Nothing -> Nothing -- The lookup stops at a grammar call
        Just v -> Just v
    lookupCtrl (ManyFrame _ _ : rest) = lookupCtrl rest
    lookupCtrl _ = error "Case not handled"

applyBinop :: BinOp -> Val -> Val -> Val
applyBinop = Interp.evalBinOp

isSimpleVExpr :: NVExpr -> Bool
isSimpleVExpr e =
  case texprValue e of
    TCCoerce _ _ _ -> False
    TCLiteral (LNumber {}) _ -> True
    TCLiteral (LByte   {}) _ -> True
    TCLiteral _            _ -> False
    TCVar _nname -> True
    TCNothing _ty -> False
    TCStruct _lst _ -> False
    TCMapEmpty _ty -> False
    TCIn _lbl _e1 _ty -> False
    TCBinOp _binop _e1 _e2 _t -> False
    TCUniOp _uniop _e1 -> False
    TCSelStruct _e1 _n _ty -> False
    TCIf _e1 _e2 _e3 -> False
    TCCall _fname _t _lst -> False
    TCFor _ -> False
    TCArray _lste _ty -> False
    x -> error ("TODO: "++ show x)

defaultValue :: Val
defaultValue = Interp.VStruct []

evalLiteral :: Literal -> Type -> Val
evalLiteral lit t =
  case lit of
    LFloating x ->
      case t of
        Type TFloat -> Interp.VFloat (double2Float x)
        Type TDouble -> Interp.VDouble x
        _ -> error "Unexpected float literal"

    LPi ->
      case t of
        Type TFloat -> Interp.vFloatPi
        Type TDouble -> Interp.vDoublePi
        _ -> error "Unexpected type for PI"

    LNumber n _ ->
      case t of
        Type (TUInt (Type (TNum m))) -> Interp.VUInt (fromIntegral m) n
        Type (TInteger)              -> Interp.VInteger n
        _ -> error "TODO: more type for integer"
    LBool b   -> Interp.VBool b
    LBytes bs ->
      Interp.VArray (Vector.fromList (map (\w -> Interp.VUInt 8 (fromIntegral w)) (BS.unpack bs)))
    LByte w _ -> Interp.VUInt 8 (fromIntegral w)




evalVExpr :: GblFuns -> NVExpr -> ControlData -> SemanticData -> Val
evalVExpr gbl expr ctrl out =
  let
    eval :: Show a => Maybe [(Name, Interp.Value)] -> TC a Value -> Interp.Value
    eval env e =
      case texprValue e of
        TCCoerce _ty1 ty2 e1 ->
          let ev = eval env e1 in
          fst $ Interp.vCoerceTo (Interp.evalType Interp.emptyEnv ty2) ev
        TCLiteral lit ty -> evalLiteral lit ty
        TCNothing _ty ->
          Interp.VMaybe (Nothing)
        TCJust e1 ->
          let ve1 = eval env e1
          in Interp.VMaybe (Just ve1)
        TCStruct lst _ ->
          Interp.VStruct (map (\ (lbl, v) -> (lbl, eval env v)) lst)
        TCMapEmpty _ty -> Interp.VMap (Map.empty)
        TCIn lbl e1 _ty ->
          let ve1 = eval env e1 in
          Interp.VUnionElem lbl ve1
        TCBinOp binop e1 e2 _ ->
          let ve1 = eval env e1
              ve2 = eval env e2
          in
            applyBinop binop ve1 ve2
        TCUniOp uniop e1 -> Interp.evalUniOp uniop (eval env e1)
        TCSelStruct e1 n _ty ->
          let ve1 = eval env e1
          in case ve1 of
               Interp.VStruct lst -> snd $ head $ filter (\ (a,_) -> a == n) lst
               _ -> error "SEl must be on Struct"
        TCIf e1 e2 e3 ->
          case eval env e1 of
            Interp.VBool True -> eval env e2
            Interp.VBool False -> eval env e3
            _ -> error "cond must be bool"
        TCCall fname _ lst ->
          let elst = map (\ expre ->
                            case expre of
                              ValArg va -> eval env va
                              _ -> error "unexpected non value argument"
                         ) lst
              fname1 = tcName fname
              argsAndBody = Map.lookup fname1 gbl
              (env1, body) =
                case argsAndBody of
                  Nothing -> error ("Fun '" ++ show fname ++ "' not found!")
                  Just (args, bdy) ->
                    let lstArgsValues = zip args elst in
                    (lstArgsValues, bdy)
          in case body of
               ValueExpr bdy -> eval (Just env1) bdy
               _ -> error "Unexpected body type, not a ValueExpression"
        TCFor lp ->
          case loopFlav lp of
            Fold n1 e1 ->
              let n2 = loopElName lp
                  e2 = loopCol lp
                  e3 = loopBody lp
              in
                -- NVFor n1 e1 n2 e2 e3 ->
              case eval env e2 of
                Interp.VArray vect ->
                  Vector.foldl f (eval env e1) vect
                  where
                    f b a =
                      let localenv = concatLocalEnv (Just ([(tcName n1, b), (tcName n2, a)])) env
                      in eval localenv e3
                _ -> error "Unexpected type in For"
            LoopMap -> error "Unhandled LoopMap"
        TCVar nname ->
          case env of
            Just env1 -> -- inside a function call
              case lookupLocalEnv (tcName nname) env1 of
                Nothing -> error ("variable not bound by function: " ++ (show nname))
                Just v -> v
            Nothing -> -- not inside a function call
              lookupEnvName (tcName nname) ctrl out
        TCArray lste _ty ->
          let lev = map (\ ex -> eval env ex) lste
          in Interp.VArray (Vector.fromList lev)
        TCArrayLength e1 ->
          let ev = eval env e1
          in
          case ev of
            Interp.VArray v -> Interp.VUInt 64 (fromIntegral $ length v)
            _ -> error "should be an array"
        TCCase e1 lpat defaultp ->
          loop (NonEmpty.toList lpat)

          where
            ev = eval env e1
            loop pat =
              case pat of
                [] ->
                  case defaultp of
                    Nothing -> error "No match found"
                    Just e2 -> eval env e2
                p : rest ->
                  case evalCase gbl ev (Just $ tcAltPatterns p) of
                    Nothing -> loop rest
                    Just m ->
                      let newEnv = concatLocalEnv (Just (Map.assocs m)) env
                      in eval newEnv (tcAltBody p)

        TCLet x e1 e2 ->
          eval (concatLocalEnv (Just [(tcName x,eval env e1)]) env) e2

        x -> error ("TODO: "++ show x)

  in eval Nothing expr
  where
    concatLocalEnv env1 env2 =
      case (env1, env2) of
        (Just env11, Just env22) -> Just (env11 ++ env22)
        (Just _, Nothing) -> env1
        (Nothing, Just _) -> env2
        (Nothing, Nothing) -> Nothing
    lookupLocalEnv n1 env =
      foldl
      (\b (n2, v) ->
         case b of
           Nothing -> if n1 == n2 then Just v else Nothing
           Just v1 -> Just v1
      ) Nothing env

evalNoFunCall:: NVExpr -> ControlData -> SemanticData -> Val
evalNoFunCall e ctrl out = evalVExpr (Map.empty) e ctrl out



valToInt :: Val -> Int
valToInt v =
  case v of
    Interp.VUInt _ i  -> toEnum (fromIntegral i ::Int)
    Interp.VSInt _ i  -> toEnum (fromIntegral i ::Int)
    Interp.VInteger i -> fromIntegral i :: Int
    _ -> error "valToInt pb"

evalCase :: GblFuns -> Val -> Maybe [TCPat] -> Maybe (Map.Map Name Val)
evalCase _gbl v lpat =
  case lpat of
    Nothing -> Just (Map.empty)
    Just pats ->
      iterateOnPats pats

  where
    iterateOnPats pats =
      case pats of
        [] -> Nothing
        p : ps ->
          case tryPat p of
            Nothing -> iterateOnPats ps
            r@(Just _) -> r

    tryPat pat =
      case pat of
        TCConPat _ lbl binding ->
          case v of
            Interp.VUnionElem lbl1 e2 ->
              if lbl1 == lbl
              then case binding of
                     TCVarPat n ->
                       Just (Map.insert (tcName n) e2 Map.empty)
                     TCWildPat _ -> Just Map.empty
                     _ -> error "not handled"
              else Nothing
            _ -> error "type error: should be a union"
        TCJustPat (TCVarPat n) ->
          case v of
            Interp.VMaybe Nothing -> Nothing
            Interp.VMaybe (Just v1) -> Just (Map.insert (tcName n) v1 Map.empty)
            _ -> error "type error: should be a Maybe"
        TCNothingPat _ ->
          case v of
            Interp.VMaybe Nothing -> Just (Map.empty)
            Interp.VMaybe (Just _) -> Nothing
            _ -> error "type error: should be a Maybe"
        TCBoolPat b ->
          case v of
            Interp.VBool b1 ->
              if b1 == b
              then Just Map.empty
              else Nothing
            _ -> error "type error: should be a Bool"
        _ -> error ("TODO not handled pat" ++ show pat)

callCExpr :: GblFuns -> NCExpr -> NCExpr
callCExpr gbl expr =
  let e = texprValue expr in
  case e of
    TCCall cname _ _lst -> -- this code is a simplification of TCVCall
      let
        cname1 = tcName cname
        argsAndBody = Map.lookup cname1 gbl
        body =
          case argsAndBody of
            Nothing -> error ("Class Fun '" ++ show cname1 ++ "' not found!")
            Just (_args, bdy) -> bdy
      in
      case body of
        ClassExpr bdy -> bdy
        _ -> error "Unexpected body type not a ClassExpression"
    _ -> error $ "Broken invariant: this function can only be called on TCCall:" ++ show e


evalCExpr :: GblFuns -> NCExpr -> Word8 -> ControlData -> SemanticData -> Maybe SemanticElm
evalCExpr gbl expr x ctrl out =
  case texprValue expr of
    TCSetSingle e     ->
      let v = evalNoFunCall e ctrl out in
      let j =  valToInt v in
        if  x == toEnum j
        then let o = SEVal v
             in Just o
        else Nothing
    TCSetComplement e     ->
      case evalCExpr gbl e x ctrl out of
        Nothing -> Just (SEVal (Interp.VUInt 8 (fromIntegral x)))
        Just _ -> Nothing
    TCSetUnion lst     ->
      let go l =
            case l of
              []   -> Nothing
              e:es ->
                let v = evalCExpr gbl e x ctrl out in
                  case v of
                    Nothing -> go es
                    Just o -> Just o
      in go lst
    TCSetOneOf bs     ->
      if elem x (BS.unpack bs)
      then Just (SEVal (Interp.VUInt 8 (fromIntegral x)))
      else Nothing
    TCSetRange e1 e2  ->
      let v1 = evalNoFunCall e1 ctrl out
          v2 = evalNoFunCall e2 ctrl out
      in let j1 = valToInt v1
             j2 = valToInt v2
         in if toEnum j1 <= x && x <= toEnum j2
            then let o = SEVal (Interp.VUInt 8 (fromIntegral x))
                 in Just o
            else Nothing
    TCCall {} -> -- this code is a simplification of TCVCall
      let calledClass = callCExpr gbl expr
      in evalCExpr gbl calledClass x ctrl out

    _ -> error ("TODO: " ++ (show expr))


-- copy from rts-hs/src/RTS/ParserAPI.hs
-- | Limit the input to the given number of bytes.
-- Fails if there aren't enough bytes.
limitLen :: Integer -> Input -> Maybe Input
limitLen n = Input.limitLen (intToSize (fromInteger n))

-- copy from rts-hs/src/RTS/ParserAPI.hs
-- Fails if we don't have enough bytes, although it is ok to
-- get to the very end of the input.
advanceBy :: Integer -> Input -> Maybe Input
advanceBy n = Input.advanceBy (intToSize (fromInteger n))

applyInputAction :: GblFuns -> (InputData, ControlData, SemanticData) -> InputAction -> Maybe (InputData, SemanticData)
applyInputAction gbl (inp, ctrl, out) act =
  case act of
    ClssAct s e ->
      case getByte inp of
        Nothing -> Nothing
        Just (x, xs) ->
          let b = evalCExpr gbl e x ctrl out
          in case b of
               Nothing -> Nothing
               Just o -> resultWithSem s xs o
    IEnd ->
      case getByte inp of
        Nothing -> Just (inp, SEVal defaultValue : out)
        _ -> Nothing
    IOffset -> Just (inp, SEVal (Interp.VInteger (fromIntegral (inputOffset inp)))  : out)
    IGetByte s ->
      case getByte inp of
        Nothing -> Nothing
        Just (x, xs) -> resultWithSem s xs (SEVal (Interp.VUInt 8 (fromIntegral x)))
    IMatchBytes s e1     ->
      let ev1 = evalVExpr gbl e1 ctrl out
      in case ev1 of
           Interp.VArray vec ->
             let len = fromIntegral (Vector.length vec)
             in
             case limitLen len inp of
               Nothing -> Nothing
               Just inp1 ->
                 if BS.unpack (Input.inputBytes inp1) == map (\v -> toEnum (valToInt v)) (Vector.toList vec)
                 then let i = fromJust $ advanceBy len inp
                          o = SEVal (Interp.VArray vec)
                      in resultWithSem s i o
                 else Nothing
           _ -> error ("unexpected match bytes: "++ show e1)
    GetStream ->
      Just (inp, SEVal (Interp.VStream inp) : out)
    SetStream e1 ->
      let ev1 = evalVExpr gbl e1 ctrl out
      in case ev1 of
           Interp.VStream i1 -> Just (i1, SEVal (defaultValue) {- technically just for an invariant at the EnvStore handling -} : out)
           _ -> error "Not an input stream at this value"
    StreamTake s e1 e2 ->
      let n   = Interp.valueToIntegral (evalVExpr gbl e1 ctrl out)
          ev2 = evalVExpr gbl e2 ctrl out
      in case ev2 of
           Interp.VStream i1 ->
             case limitLen n i1 of
               Nothing -> Nothing
               Just i2 -> resultWithSem s inp (SEVal (Interp.VStream i2))
           _ -> error "Not an input stream"


    StreamDrop s e1 e2 ->
      let n   = Interp.valueToIntegral (evalVExpr gbl e1 ctrl out)
          ev2 = evalVExpr gbl e2 ctrl out
      in case ev2 of
           Interp.VStream i1 ->
             case advanceBy n i1 of
               Nothing -> error "stream too short for advance"
               Just i2 -> resultWithSem s inp (SEVal (Interp.VStream i2))
           _ -> error "Not an input stream"
  where
    resultWithSem YesSem i o = Just (i, o : out)
    resultWithSem NoSem  i _ = Just (i, SEVal (defaultValue) : out)


initSemanticData :: SemanticData
initSemanticData = []

applyControlAction :: GblFuns -> (ControlData, SemanticData) -> ControlAction -> Maybe (ControlData, SemanticData)
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
    BoundCheckMore ->
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
    Pop -> error "Should not be handled here"
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

    -- TODO: reorder `zip lvs ln` to `zip ln lvs`
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
        _ -> error "unexpected ctrl"
      )
    CaseCall e1 ->
      let ev1 = evalVExpr gbl e1 ctrl out
          casefrm = CaseOn ev1 out
      in Just (CaseFrame casefrm : ctrl, initSemanticData)
    CaseTry pat ->
      case (ctrl, out) of
        (CaseFrame (CaseOn {caseOn = v, caseSavedOut = cout}) : rest, []) ->
          let mres = evalCase gbl v pat in
            case mres of
              Nothing -> Nothing
              Just m -> Just ((CaseFrame (CaseBound m cout)) : rest, out)
        _ -> error "unexpected ctrl"
    CaseEnd ->
      case (ctrl, out) of
        (CaseFrame (CaseBound { caseSavedOut = sout}) : rest, [v]) ->
          Just (rest, v : sout)
        _ -> error "unexpected ctrl"


applySemanticAction :: GblFuns -> (ControlData, SemanticData) -> SemanticAction -> Maybe SemanticData
applySemanticAction gbl (ctrl, out) act =
  case act of
    EnvFresh -> Just (SEnvMap Map.empty : out)
    ManyFreshList s ->
      case s of
        YesSem -> Just (SManyVal [] : out)
        NoSem  -> Just (SManyVal [] : out)
    ManyAppend YesSem ->
      case out of
        x : (SManyVal y) : z  -> (
          case x of
            SEVal v -> Just ((SManyVal (v : y)) : z)
            _ -> error "unexpected out, cannot proceed"
          )
        _ -> error ("unexpected out, cannot proceed: " ++ show out)
    ManyAppend NoSem ->
      case out of
        _ : e@((SManyVal []) : _)  -> Just e
        _ -> error "unexpected out, cannot proceed"
    ManyReturn ->
      case out of
        SManyVal lst : y -> Just $ SEVal (Interp.VArray (Vector.fromList (reverse lst))) : y
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
            SManyVal _ -> error "unexpected out, cannot proceed"
            SEnd -> Just rest
          )
        _ -> error ("unexpected out, cannot proceed: " ++
                    show mname ++ "\nOUT:\n" ++ show out ++ "\nCTRL:\n" ++ show ctrl)
      )
    EvalPure e -> Just (SEVal (evalVExpr gbl e ctrl out) : out)
    ReturnBind e -> Just (SEVal (evalVExpr gbl e ctrl out) : tail out)
    ReturnLast -> Just (head out : tail (tail out))
    DropOneOut ->
      case out of
        [] -> error "Should not Happen: drop on empty sem stack"
        _ : os -> Just os
    MapLookup s e1 e2 ->
      let ev1 = evalVExpr gbl e1 ctrl out
          ev2 = evalVExpr gbl e2 ctrl out
      in case ev2 of
           Interp.VMap m -> case Map.lookup ev1 m of
                             Nothing -> Nothing
                             Just v -> resultWithSem s v
           _ -> error "Lookup is not applied to value of type map"
    MapInsert s e1 e2 e3 ->
      let ev1 = evalVExpr gbl e1 ctrl out
          ev2 = evalVExpr gbl e2 ctrl out
          ev3 = evalVExpr gbl e3 ctrl out
      in case ev3 of
           Interp.VMap m ->
             if Map.member ev1 m
             then Nothing
             else resultWithSem s (Interp.VMap (Map.insert ev1 ev2 m))
           _ -> error "Lookup is not applied to value of type map"
    CoerceCheck s _t1 t2 e1 ->
      let ev1 = evalVExpr gbl e1 ctrl out in
        case Interp.vCoerceTo (Interp.evalType Interp.emptyEnv t2) ev1 of
          (v, True) -> resultWithSem s v
          (_, False) -> Nothing -- error "Lossy coercion"

    Guard e1 ->
      let ev1 = evalVExpr gbl e1 ctrl out
      in case ev1 of
           Interp.VBool b -> if b then Just (SEVal (defaultValue) : out) else Nothing
           _ -> error "Guard must evaluate to a boolean"

  where
    resultWithSem YesSem o = Just (SEVal o : out)
    resultWithSem NoSem  _ = Just (SEVal (defaultValue) : out)


applyAction :: GblFuns -> (InputData, ControlData, SemanticData) -> State -> Action ->
  Maybe (InputData, ControlData, SemanticData, State)
applyAction gbl (inp, ctrl, out) q2 act =
  case act of
    EpsA       -> Just (inp, ctrl, out, q2)
    IAct a   -> case applyInputAction gbl (inp, ctrl, out) a of
                    Nothing -> Nothing
                    Just (inp1, out1) -> Just (inp1, ctrl, out1, q2)
    CAct a     -> case a of
                    Pop ->
                      case ctrl of
                        CallFrame _ q' _ savedOut : rest -> Just (inp, rest, head out : savedOut, q')
                        _ -> error "broken invariant of Pop"
                    _ ->
                      case applyControlAction gbl (ctrl, out) a of
                        Nothing -> Nothing
                        Just (ctrl1, out1) -> Just (inp, ctrl1, out1, q2)
    SAct a     -> case applySemanticAction gbl (ctrl, out) a of
                    Nothing -> Nothing
                    Just out1 -> Just (inp, ctrl, out1, q2)
    BAct _     -> error "This case should not be handled in applyAction"
