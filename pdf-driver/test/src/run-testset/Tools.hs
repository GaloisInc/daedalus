module Tools where

-- system:
import           Data.List
import           System.Console.GetOpt
import           System.Exit
import           Text.Read(readMaybe)

-- regex-tdfa pkg:
import qualified Text.Regex.TDFA as RE

-- local:
import Types
import Util


---- tools -------------------------------------------------------------------

tools :: [Tool]
tools = [validate_T, totext_T, cmapSimple_T, cmapCid_T]

toolnames :: [String]
toolnames = map t_name tools
  
toolnameRegEx :: String
toolnameRegEx = "("++ intercalate "|" toolnames ++ ")"

---- tool: validate ----

validate_T :: Tool
validate_T =
  T { t_name          = "validatePDF"
    , t_cmd_exec      = "pdf-hs-driver"
    , t_cmd_mkArgs    = (\f->[f])
    , t_timeoutInSecs = 4*60  -- typically overridden on command-line
    , t_proj          = proj
    , t_cmp           = cmp
    }

  where
  -- NB: the result-expected data is a RegExp!
  cmp re' actual' =
    return $ if matchesRE re actual then
               Equivalent
             else
               NotEquivalent msg
      
    where
    re = rmTrailingWhitespace re'
    actual = rmTrailingWhitespace actual'
    res = split '|' re
    info = concat ["driver gives '", actual, "', but expected is '",re,"'"]
    pinfo = "(" ++ info ++ ")"
    msg =
      case (re,actual) of
        ("NCBUR","Good") -> "semi false-negative: " ++ pinfo
        ("NCBUR","Bad" ) -> "semi false-positive: " ++ pinfo
        (_      ,"Good") | "Bad"  `elem` res -> "false-negative: " ++ pinfo
        (_      ,"Bad" ) | "Good" `elem` res -> "false-positive: " ++ pinfo
        _                                    -> info                   
      
  matchesRE :: String -> String -> Bool
  matchesRE expectedRegEx actual =
    RE.matchTest (regExpNotCS expectedRegEx) actual
    where
    regExpNotCS :: String -> RE.Regex
    regExpNotCS s =
      RE.makeRegexOpts RE.defaultCompOpt{RE.caseSensitive=False}
                       RE.defaultExecOpt
                       s
                
  proj :: IO String -> IO String -> IO MetaData-> IO String
  proj _getStdOut getStdErr getMeta =
    do
    errs  <- lines <$> getStdErr
    metaD <- getMeta
    let r = case errs of
              ["ACCEPT",_] -> Good
              ["REJECT",_] -> Bad
              []           -> if exitCode metaD == ExitSuccess then
                                Unclear
                              else
                                Timeout
              _            -> Unclear
              
    return (show r)

data Result_validate_T =
       Good
     | Bad
     | Timeout
     | Unclear   -- the result is unclear, apparently inconsistent output
     | NCBUR     -- future (no info at the moment from pdf-hs-driver)
     deriving (Eq, Read, Show)  


---- tools: two cmap tools ----

cmapSimple_T :: Tool
cmapSimple_T =
  T { t_name          = "cmap-sf"
    , t_cmd_exec      = "pdf-dom"
    , t_cmd_mkArgs    = (\f->["--parse-type=ToUnicodeCMap_simpleFont", f])
    , t_timeoutInSecs = 30
    , t_proj          = proj
    , t_cmp           = cmp
    }

  where
    
  proj :: IO String -> IO String -> IO MetaData-> IO String
  proj getStdOut _getErr _getMeta = getStdOut
  
  cmp expected actual =
    return $ boolToCompared (expected == actual) -- FIXME[F1]: must relax!
  
cmapCid_T :: Tool
cmapCid_T =
  T { t_name          = "cmap-cid"
    , t_cmd_exec      = "pdf-dom"
    , t_cmd_mkArgs    = (\f->["--parse-type=ToUnicodeCMap_cidFont", f])
    , t_timeoutInSecs = 30
    , t_proj          = proj
    , t_cmp           = cmp
    }

  where
    
  proj :: IO String -> IO String -> IO MetaData-> IO String
  proj getStdOut _getErr _getMeta = getStdOut
  
  cmp expected actual =
    return $ boolToCompared (expected == actual) -- FIXME[F1]: must relax!
  

---- tool: totext ----

totext_T :: Tool
totext_T =
  T { t_name          = "totext"
    , t_cmd_exec      = "pdf-hs-driver"
    , t_cmd_mkArgs    = (\f->["-t", f])
    , t_timeoutInSecs = 3*60
    , t_proj          = proj
    , t_cmp           = cmp
    }

  where
    
  proj :: IO String -> IO String -> IO MetaData-> IO String
  proj getStdOut _getErr _getMeta =
    unlines . filter isText . lines <$> getStdOut
    where
    isText []                           = False   
    isText s | "INFO - " `isPrefixOf` s = False
             | otherwise                = True

  cmp expected actual =
    return $ boolToCompared (expected == actual) -- FIXME[F1]: must relax!
    -- note that we do unicode 'ff' while pdftotext does 2 'f' chars

    
---- options -----------------------------------------------------------------

data Flags = F_ToolName String
           | F_CorporaName String
           | F_Timeout Int
           deriving (Eq,Show,Ord)

options =
  [ Option []  ["tool"]    (ReqArg mkToolName    toolnameRegEx)
      "tool name (REQUIRED)"
  , Option []  ["corpora"] (ReqArg mkCorporaName "CORP"       )
      "corpora name (REQUIRED)"
  , Option "t" ["timeout"] (ReqArg (fmap F_Timeout . mkIntegerArg) "sec")
      "timeout"
  ]
  where
  mkCorporaName s = Right (F_CorporaName s)
  
  mkToolName s
    | s `elem` toolnames = Right (F_ToolName s)
    | otherwise          = Left $ "toolname must be '" ++ toolnameRegEx ++ "'"
  
  mkIntegerArg s = case readMaybe s of
                     Just i  -> Right i
                     Nothing -> Left "can't parse integer"

data Options = Options { tool     :: Tool
                       , corpName :: String
                       , timeOut  :: Maybe Int  -- in secs!
                       }

toolName :: Options -> String
toolName (Options{tool=T{t_name=s}}) = s

usage = quit (usageInfo "Usage: run-testset [OPTION...]\nOPTIONS:" options)
                     
processFlags :: [Flags] -> IO Options
processFlags flags =
  do
  (toolName,corpName') <- 
    case ([cn | F_CorporaName cn <- flags]
         ,[tn | F_ToolName tn <- flags]
         )
    of
      ([cn],[tn]) -> return (tn,cn)
      _           -> usage
    
  let to = case [s | F_Timeout s <- flags] of
             [] -> Nothing
             ts -> Just (last ts)
             
  t <- case [ t | t <- tools, t_name t == toolName ] of
         [x] -> return x
         []  -> fail $ "not a valid tool name: " ++ toolName
         _   -> error "in 'tools/runTest'"
  return $ Options {tool=t, corpName=corpName', timeOut=to}
