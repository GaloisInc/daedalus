-- Assumes borrow analysis has been done
-- Insert a copy when we need to pass a value as an owned argument.
module Daedalus.VM.InsertCopy where

import Daedalus.VM
import Daedalus.VM.BorrowAnalysis

instruction instr =
  case instr of
    SetInput e      -> owned SetInput e
    Say {}          -> emit instr
    Output e        -> undefined
    Notify e        -> undefined
    CallPrim x p es -> undefined
    GetInput x      -> undefined
    Spawn x l       -> undefined
    NoteFail        -> undefined
    Let x e         -> undefined
    Free x          -> undefined

  where
  owned f e = do e1 <- copy e
                 emit (f e1)

copy :: E -> M E
copy = undefined

--------------------------------------------------------------------------------
type M = IO

emit :: Instr -> M ()
emit = undefined

