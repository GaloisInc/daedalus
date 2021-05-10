-- If we have a function which tail calls itself we turn it into a loop.
-- XXX: More generally, if we have a mutually recursive group of functions
-- which tail call each other, we could put them in a single function and
-- jump instead.
-- XXX: This duplicates stuff in FindLoops.
module Daedalus.VM.TailCallJump where

import Daedalus.VM


tailProgram :: Program -> Program
tailProgram p = p { pModules = tailModule <$> pModules p }

tailModule :: Module -> Module
tailModule m = m { mFuns = tailFun <$> mFuns m }

tailFun :: VMFun -> VMFun
tailFun fun = fun { vmfBlocks = checkBlock <$> vmfBlocks fun }
  where
  checkBlock b =
    case blockTerm b of
      TailCall f _ es | f == vmfName fun -> b { blockTerm = Jump jp }
        where jp = JumpPoint { jLabel = vmfEntry fun
                             , jArgs  = es
                             }
      _ -> b

