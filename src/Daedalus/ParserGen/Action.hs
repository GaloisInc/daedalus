{-# Language GADTs, DataKinds, ExistentialQuantification, RecordWildCards #-}

module Daedalus.ParserGen.Action where

-- import Debug.Trace

import qualified Data.Map as Map
import Data.Word
import qualified Data.Vector as Vector
import Data.Maybe (fromJust)
import qualified Data.ByteString as BS
import Data.Text(Text)
import Data.Bits(shiftL,shiftR,(.|.),(.&.),xor)

import Daedalus.PP hiding (empty)

import Daedalus.Type.AST
import qualified Daedalus.Interp as Interp
import Daedalus.Interp.Value (valueToByteString, doCoerceTo)
import RTS.Input(Input(..), newInput)
import qualified RTS.Input as Input

import Daedalus.ParserGen.AST (CorV(..), GblFuns, NVExpr, NCExpr)
import Daedalus.ParserGen.ClassInterval (ClassInterval(..), IntervalEndpoint(..))

type State = Int


data InputAction =
    ClssItv ClassInterval -- this case comes from the determinization from LL(*)
  | ClssAct WithSem NCExpr
  | IEnd
  | IOffset
  | IGetByte WithSem
  | IMatchBytes WithSem NVExpr
  | GetStream
  | SetStream NVExpr
  | StreamLen WithSem NVExpr NVExpr
  | StreamOff WithSem NVExpr NVExpr


data ControlAction =
    BoundSetup (ManyBounds NVExpr)
  | BoundCheckSuccess
  | BoundIncr
  | BoundIsMore
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
  | SelUnion    WithSem NVExpr Label
  | SelJust     WithSem NVExpr
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


showName :: Name -> String
showName n =
  showName1 (nameScopedIdent n)
  where
    showName1 ((ModScope _ x)) = show x
    showName1 ((Unknown x)) = show x
    showName1 ((Local x)) = show x


instance Show(InputAction) where
  show (ClssItv _)    = "ClssItv"
  show (ClssAct s _)  = semToString s ++ "Match"
  show (IEnd)         = "END"
  show (IOffset)      = "IOffset"
  show (IGetByte s)   = semToString s ++ "GetByte"
  show (IMatchBytes s e) = semToString s ++ "MatchBytes " ++ show (pp $ texprValue e)
  show (GetStream)       = "GetStream"
  show (SetStream _)     = "SetStream"
  show (StreamLen s _ _) = semToString s ++ "StreamLen"
  show (StreamOff s _ _) = semToString s ++ "StreamOff"

instance Show(ControlAction) where
  show (BoundSetup _)      = "BoundSetup"
  show (BoundCheckSuccess) = "BoundCheckSuccess"
  show (BoundIncr)         = "BoundIncr"
  show (BoundIsMore)       = "BoundIsMore"
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
  show (SelUnion    _x _y _lbl)  = "SelUnion" -- ++ " " ++ show x ++ " " ++ show y ++ " " ++ show lbl
  show (SelJust     _ _)    = "SelJust"
  show (Guard       _)      = "Guard"


instance Show(BranchAction) where
  show (CutBiasAlt _) = "CutBiasAlt"
  show (CutLocal)     = "CutLocal"
  show (CutGlobal)    = "CutGlobal"
  show (FailAction _) = "FailAction"

instance Show(Action) where
  show EpsA             = "eps"
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

isNonClassInputAct :: Action -> Bool
isNonClassInputAct act =
  case act of
    IAct iact ->
      case iact of
        ClssAct _ _ -> False
        GetStream -> False
        SetStream _ -> False
        StreamLen {} -> False
        StreamOff {} -> False
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
        _ -> False
    SAct sact ->
      case sact of
        MapInsert {} -> True
        SelUnion {} -> True
        Guard _ -> True
        CoerceCheck {} -> True
        SelJust {} -> True
        _ -> False
    _ -> False

isBranchAction :: Action -> Bool
isBranchAction act =
  case act of
    BAct _ -> True
    _ -> False

getClassActOrEnd :: Action -> Either NCExpr InputAction
getClassActOrEnd act =
  case act of
    IAct iact ->
      case iact of
        ClssAct _ ca -> Left ca
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

getByteArray :: NVExpr -> Maybe [Word8]
getByteArray e =
  case texprValue e of
    TCLiteral (LBytes w) _ -> Just (BS.unpack w)
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

data ControlElm =
    ManyFrame !(BetweenItv) {-# UNPACK #-} !Int -- the integer is the current counter
  | ForFrame  !ForFrm
  | MapFrame  !MapFrm
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
        CallFrame name _ _ _ -> showCallStack xs ++ "\n" ++ spacing ++ showName name
        _ -> showCallStack xs
  where spacing = "  "


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
        (Interp.VStruct a1, Interp.VStruct a2) ->
          Interp.VBool $ iterateTest a1 a2
          where iterateTest arr1 arr2 =
                  case (arr1, arr2) of
                    ([], []) -> True
                    ( _ : _, []) -> False
                    ([], _ : _) -> False
                    ((lbl1, v1): r1, (lbl2, v2) : r2) ->
                      if lbl1 == lbl2
                      then case applyBinop Eq v1 v2 of
                             Interp.VBool b -> if not b then False
                                               else iterateTest r1 r2
                             _ -> error "Eq test cannot return something else than a boolean"
                      else False
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
    Cat ->
      case (e1, e2) of
        (Interp.VUInt p1 v1, Interp.VUInt p2 v2) ->
          Interp.VUInt (p1 + p2) ((v1 `shiftL` fromIntegral p2) .|. v2)
        _ -> error ("Impossible values: " ++ show op ++ show (e1,e2))
    LCat ->
      -- copied from Interp.hs
      case e2 of
        Interp.VUInt w y ->
          let mk f i = f ((i `shiftL` fromIntegral w) .|. y)
              --- XXX: fromIntegral is a bit wrong
          in
          case e1 of
            Interp.VInteger x -> mk Interp.VInteger  x
            Interp.VUInt n x  -> mk (Interp.VUInt n) x
            Interp.VSInt n x  -> mk (Interp.VSInt n) x
            _          -> error "BUG: 1st argument to (<#) must be numeric"
        _ -> error "BUG: 2nd argument of (<#) should be UInt"


    _ -> error ("TODO: " ++ show op)


name2Text :: Name -> Text
name2Text n =
  let x = nameScopedIdent n
  in case x of
    Unknown ident -> ident
    Local   ident -> ident
    ModScope _ ident -> ident

-- name2Text2 :: PAST.NName -> Text
-- name2Text2 n =
--   let x = nameScope (PAST.nName n)
--   in case x of
--     Unknown ident -> ident
--     Local   ident -> ident
--     ModScope _ ident -> ident


tODOCONST :: Integer
tODOCONST = 2


isSimpleVExpr :: NVExpr -> Bool
isSimpleVExpr e =
  case texprValue e of
    TCCoerce _ _ _ -> False
    TCLiteral (LNumber {}) _ -> True
    TCLiteral (LByte   {}) _ -> True
    TCLiteral _            _ -> False
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
    TCVar _nname -> False
    TCArray _lste _ty -> False
    TCUnit -> False
    x -> error ("TODO: "++ show x)

defaultValue :: Val
defaultValue = Interp.VStruct []

evalLiteral :: Literal -> Type -> Val
evalLiteral lit t =
  case lit of
    LNumber n ->
      case t of
        Type (TUInt (Type (TNum m))) -> Interp.VUInt (fromIntegral m) n
        Type (TInteger)              -> Interp.VInteger n
        _ -> error "TODO: more type for integer"
    LBool b   -> Interp.VBool b
    LBytes bs ->
      Interp.VArray (Vector.fromList (map (\w -> Interp.VUInt 8 (fromIntegral w)) (BS.unpack bs)))
    LByte w -> Interp.VUInt 8 (fromIntegral w)




evalVExpr :: GblFuns -> NVExpr -> ControlData -> SemanticData -> Val
evalVExpr gbl expr ctrl out =
  let
    eval :: Show a => Maybe [(Name, Interp.Value)] -> TC a Value -> Interp.Value
    eval env e =
      case texprValue e of
        TCCoerce _ty1 ty2 e1 ->
          let ev = eval env e1 in
          fst $ doCoerceTo (Interp.evalType Interp.emptyEnv ty2) ev
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
        TCUniOp uniop e1 ->
          let ve1 = eval env e1
          in case (ve1, uniop) of
            (Interp.VBool b, Not) -> Interp.VBool (not b)
            (Interp.VInteger i, Neg) -> Interp.VInteger (negate i)
            (Interp.VArray v, Concat) ->
              Interp.VArray
              (Vector.fromList
                (Vector.foldr (
                    \ a b ->
                      case a of
                        Interp.VArray av -> Vector.toList av ++ b
                        _ -> error "element in Array concatenation is not an Array")
                  [] v
                )
              )
            x -> error ("not Integer type" ++ show x ++ show uniop)
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
                    where f b a =
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
        TCUnit -> defaultValue
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

evalNoFunCall:: NVExpr -> ControlData -> SemanticData -> Val
evalNoFunCall e ctrl out = evalVExpr (Map.empty) e ctrl out



valToInt :: Val -> Int
valToInt v =
  case v of
    Interp.VUInt 8 i -> toEnum (fromIntegral i ::Int)
    Interp.VInteger i -> fromIntegral i :: Int
    _ -> error "valToInt pb"

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
    TCCall cname _ _lst -> -- this code is a simplification of TCVCall
      let cname1 = tcName cname
          argsAndBody = Map.lookup cname1 gbl
          body = case argsAndBody of
                   Nothing -> error ("Class Fun '" ++ show cname1 ++ "' not found!")
                   Just (_args, bdy) -> bdy
      in case body of
           ClassExpr bdy -> evalCExpr gbl bdy x ctrl out
           _ -> error "Unexpected body type not a ClassExpression"
    _ -> error ("TODO: " ++ (show expr))


-- copy from rts-hs/src/RTS/ParserAPI.hs
-- | Limit the input to the given number of bytes.
-- Fails if there aren't enough bytes.
limitLen :: Integer -> Input -> Maybe Input
limitLen = Input.limitLen

-- copy from rts-hs/src/RTS/ParserAPI.hs
-- Fails if we don't have enough bytes, although it is ok to
-- get to the very end of the input.
advanceBy :: Integer -> Input -> Maybe Input
advanceBy = Input.advanceBy

applyInputAction :: GblFuns -> (InputData, ControlData, SemanticData) -> InputAction -> Maybe (InputData, SemanticData)
applyInputAction gbl (inp, ctrl, out) act =
  case act of
    ClssItv (ClassBtw i j) ->
      case getByte inp of
        Nothing -> Nothing
        Just (x, xs) ->
          case (i,j) of
            (CValue a, CValue b) ->
              if (a <= x) && (x <= b)
              then Just (xs, SEVal (Interp.VUInt 8 (fromIntegral x)) : out)
              else Nothing
            (_, _) -> error "Class Interval not handled"

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
    StreamLen s e1 e2 ->
      let ev1 = evalVExpr gbl e1 ctrl out
          ev2 = evalVExpr gbl e2 ctrl out
      in case ev1 of
           Interp.VInteger n ->
             case ev2 of
               Interp.VStream i1 ->
                 case limitLen n i1 of
                   Nothing -> error "stream value too short for len"
                   Just i2 -> resultWithSem s inp (SEVal (Interp.VStream i2))
               _ -> error "Not an input stream"
           _ -> error "Not an integer for Taking"
    StreamOff s e1 e2 ->
      let ev1 = evalVExpr gbl e1 ctrl out
          ev2 = evalVExpr gbl e2 ctrl out
      in case ev1 of
        Interp.VInteger n ->
          case ev2 of
            Interp.VStream i1 ->
              case advanceBy n i1 of
                Nothing -> error "stream too short for advance"
                Just i2 -> resultWithSem s inp (SEVal (Interp.VStream i2))
            _ -> error "Not an input stream"
        _ -> error "Not an integer for Taking"
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
        _ -> error "unexpected out"
      )


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
        _ -> error ("unexpected out, cannot proceed: " ++ show mname ++ "\nOUT:\n" ++ show out ++ "\nCTRL:\n" ++ show ctrl)
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
        case doCoerceTo (Interp.evalType Interp.emptyEnv t2) ev1 of
          (v, NotLossy) -> resultWithSem s v
          (_, Lossy) -> Nothing -- error "Lossy coercion"
    SelUnion s e1 lbl ->
      let ev1 = evalVExpr gbl e1 ctrl out
      in case ev1 of
           Interp.VUnionElem lbl1 e2 ->
             if lbl1 == lbl
             then resultWithSem s e2
             else Nothing
           _ -> error "Selection not a union"

    SelJust s e1 ->
      let ev1 = evalVExpr gbl e1 ctrl out
      in case ev1 of
           Interp.VMaybe Nothing -> Nothing
           Interp.VMaybe (Just v1) -> resultWithSem s v1
           _ -> error "Semantic output not a map"

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
