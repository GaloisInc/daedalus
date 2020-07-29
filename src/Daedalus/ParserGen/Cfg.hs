module Daedalus.ParserGen.Cfg where

import qualified Data.ByteString as BS

import RTS.Input(newInput)

import Daedalus.ParserGen.Action (InputData, ControlData, SemanticData, State, isEmptyControlData)
import Daedalus.ParserGen.Aut (Aut, initials, isAccepting, isAcceptingEps)

data Cfg = Cfg InputData ControlData SemanticData State
  --deriving (Eq)

instance (Show Cfg) where
  show (Cfg inp ctrl out st) =
    "in :" ++ (show inp) ++ "\n" ++
    "out:" ++ (show out) ++ "\n" ++
    "ctl:" ++ (show ctrl) ++ "\n" ++
    "st :" ++ (show st) ++ "\n"

initCfg :: BS.ByteString -> Aut -> Cfg
initCfg s aut =
  Cfg initInput initCtrl initOut (initials aut)
  where
    initOut = []
    initCtrl = []
    initInput = newInput BS.empty s

-- Decides if a configuration is accepting. input, stack and state
-- accepting.
isAcceptingCfg :: Cfg -> Aut -> Bool
isAcceptingCfg (Cfg _input _stack _output state) aut =
  -- input == [] && stack == [] && length output <= 1 && isAccepting state aut
  isEmptyControlData _stack && -- this is just a sanity check
  isAccepting state aut


-- Decides if a configuration is accepting. input, stack and state
-- accepting.
isAcceptingCfgEps :: Cfg -> Aut -> Bool
isAcceptingCfgEps (Cfg _input _stack _output state) aut =
  --input == [] && stack == [] && length output <= 1 &&
  isAcceptingEps state aut
