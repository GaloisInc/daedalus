{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Talos.Polyglot.Location where

import Daedalus.Core
import Daedalus.Core.CFG
import Daedalus.PP

-- | Function name and node ID, which specifices a location in the CFG.
type Loc = (FName, NodeID)

ppLoc :: Loc -> Doc
ppLoc (name, nodeID) = pp name <> colon <> pp nodeID

-- | The sequence of call sites leading to the current location.  Does not
-- include the current function, e.g. an empty call stack means the location is
-- in the Main function.
type CallStack = [Loc]

ppCallStack :: CallStack -> Doc
ppCallStack stack = brackets . hcat $ punctuate (text ">") (reverse $ map ppLoc stack)

ppStackLoc :: (CallStack, Loc) -> Doc
ppStackLoc (stack, loc) = text "<" <> ppCallStack stack <> text "@" <> ppLoc loc <> text ">"