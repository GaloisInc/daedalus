module Daedalus.Interp.Config where


-- | Information on how we'd like to interpret things
data InterpConfing = InterpConfing
  { multiErrors    :: Bool
    -- ^ Should we report all possible errors?   Unless this is set we only
    -- report the "best" error accoring to a heiristic.

  , detailedCallstack :: Bool
    -- ^ Should we keep details about the values in scope in the call stack

  , tracedValues   :: Bool
    -- ^ Should we keep track of which bytes in the input contributed
    -- towards various values.

  } deriving Show


-- | Default intepreter configuration.
-- Report single error, without extra detail.
defaultInterpConfig :: InterpConfing
defaultInterpConfig = InterpConfing
  { multiErrors       = False
  , detailedCallstack = False
  , tracedValues      = False
  }

-- | Enables detailed call stacks and traced values.
detailedErrorsConfig :: InterpConfing
detailedErrorsConfig = defaultInterpConfig
  { multiErrors = True
  , detailedCallstack = True
  , tracedValues = True
  }


