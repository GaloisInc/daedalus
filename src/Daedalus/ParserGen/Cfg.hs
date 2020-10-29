module Daedalus.ParserGen.Cfg where

import qualified Data.ByteString as BS

import RTS.Input(newInput)

import Daedalus.ParserGen.Action (InputData, ControlData, SemanticData, State, isEmptyControlData)
import Daedalus.ParserGen.Aut (Aut, initialState, isAcceptingState)

data Cfg = Cfg !InputData !ControlData !SemanticData {-# UNPACK #-} !State
  --deriving (Eq)

instance (Show Cfg) where
  show (Cfg inp ctrl out st) =
    "in :" ++ (show inp) ++ "\n" ++
    "out:" ++ (show out) ++ "\n" ++
    "ctl:" ++ (show ctrl) ++ "\n" ++
    "st :" ++ (show st) ++ "\n"

initCfg :: Aut a => BS.ByteString -> a -> Cfg
initCfg s aut =
  Cfg initInput initCtrl initOut (initialState aut)
  where
    initOut = []
    initCtrl = []
    initInput = newInput BS.empty s

-- Decides if a configuration is accepting. input, stack and state
-- accepting.
isAcceptingCfg :: Aut a => Cfg -> a -> Bool
isAcceptingCfg (Cfg _input _stack _output state) aut =
  -- input == [] && stack == [] && length output <= 1 && isAccepting state aut
  isEmptyControlData _stack && -- this is just a sanity check
  isAcceptingState aut state


-- Decides if a configuration is accepting. input, stack and state
-- accepting.
-- isAcceptingCfgEps :: Cfg -> Aut -> Bool
-- isAcceptingCfgEps (Cfg _input _stack _output state) aut =
--   --input == [] && stack == [] && length output <= 1 &&
--   isAcceptingEps state aut
