{-# LANGUAGE LambdaCase #-}

module Types where

-- system:
import           System.Exit


---- types, constants --------------------------------------------------------

data Tool =
  T { t_name          :: String
    , t_cmd_exec      :: FilePath
    , t_cmd_mkArgs    :: FilePath -> [String]
    , t_timeoutInSecs :: Int
    , t_proj          :: IO String   -- get stdout
                      -> IO String   -- get stderr
                      -> IO MetaData -- get metadata
                      -> IO String   -- the result of tool (as string)
                        -- FIXME: this may add unnecessary 'needs'

    , t_cmp           :: FilePath -> FilePath -> IO Compared
    }


  -- FIXME: name things as an 'isvalid' (not 'result') projection
  -- and abstract over projections (e.g., have multiple, named projs)

data ErrorType = EQ_Variance
               | NE_NoVariance
               deriving (Eq, Read, Show)

data MetaData = MetaData { exitCode :: ExitCode
                         , runtime  :: Int  -- in msecs
                         }
                deriving (Eq,Show,Read)

---- Compared ... ------------------------------------------------------------

data Compared = Equivalent
              | NotEquivalent String
               deriving (Eq, Read, Show)

isEquivalent :: Compared -> Bool
isEquivalent Equivalent = True
isEquivalent _          = False

boolToCompared b = if b then
                     Equivalent
                   else
                     NotEquivalent ""

ppCompared = \case
  Equivalent      -> "Equivalent"
  NotEquivalent s -> "NotEquivalent\n" ++ s

isPPEquivalent :: String -> Bool
isPPEquivalent s =
  case lines s of
    "Equivalent":_     -> True
    "NotEquivalent":_  -> False
    _                  -> error "malformed pretty-print of Compared"
  
