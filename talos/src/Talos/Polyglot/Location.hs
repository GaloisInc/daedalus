{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Talos.Polyglot.Location where

import qualified Data.List as List

import Daedalus.Core
import Daedalus.Core.CFG
import Daedalus.PP

-- | Function name and node ID, which specifices a location in the CFG.
type Loc = (FName, NodeID)

ppLoc :: Loc -> Doc
ppLoc (name, nodeID) = pp name <> colon <> pp nodeID

-- | This is a bit of a hack.  This distinguishes between
-- incoming edges and returning edges at a call site.
data CallSite = Loc Loc | Returning
  deriving (Eq, Ord)

instance PP CallSite where
  pp (Loc l)   = ppLoc l
  pp Returning = text "RETURNING"

-- | The sequence of call sites leading to the current location.  Does not
-- include the current function, e.g. an empty call stack means the location is
-- in the Main function.
type CallStack = [CallSite]

ppCallStack :: CallStack -> Doc
ppCallStack stack = brackets . hcat $ punctuate (text ">") (reverse $ map pp stack)

ppStackLoc :: (CallStack, Loc) -> Doc
ppStackLoc (stack, loc) = text "<" <> ppCallStack stack <> text "@" <> ppLoc loc <> text ">"

-- Push a call site if it is not already in the stack.
safePush :: CallSite -> CallStack -> CallStack
safePush site stack = if List.elem site stack then stack else site:stack