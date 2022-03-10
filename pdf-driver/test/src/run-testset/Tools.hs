module Tools where

-- system:
import           Data.List
import           System.Console.GetOpt
import           System.Exit

-- regex-tdfa pkg:
import qualified Text.Regex.TDFA as RE

-- local:
import Types


---- options -----------------------------------------------------------------

data Flags = F_ToolName String
           | F_CorporaName String
--         | F_Timeout Int
           deriving (Eq,Show,Ord)

options =
  [ Option [] ["tool"]    (ReqArg mkToolName    toolnameRegEx) "tool name"
  , Option [] ["corpora"] (ReqArg mkCorporaName "CORP"       ) "corpora name"
  -- , Option "s" ["timeout"] (ReqArg  "sec")  "timeout"
  ]
  where
  -- mkTimeout s = 
  mkCorporaName s = Right (F_CorporaName s)
  mkToolName s
    | s `elem` toolnames = Right (F_ToolName s)
    | otherwise          = Left $ "toolname must be '" ++ toolnameRegEx ++ "'"
  


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
    , t_timeoutInSecs = timeoutInSecs
    , t_proj          = proj
    , t_cmp           = matchesRE
    }

  where
  matchesRE :: String -> String -> Bool
  matchesRE expectedRegEx actual =
    RE.matchTest (regExpNotCS expectedRegEx) actual
    where
    regExpNotCS :: String -> RE.Regex
    regExpNotCS s =
      RE.makeRegexOpts RE.defaultCompOpt{RE.caseSensitive=False}
                       RE.defaultExecOpt
                       s
                
  timeoutInSecs = 4*60  -- FIXME: be able to specify on command line??
                        -- currently 8-ish files are timing out (2020-03-eval/*)
  proj :: IO String -> IO String -> IO MetaData-> IO String
  proj _getOut _getErr getMeta =
    do
    metaD <- getMeta
    let r = if runtime metaD >= timeoutInSecs*1000 then
              Timeout
            else if exitCode metaD == ExitSuccess then
              Good
            else
              Bad
    return (show r)

data Result_validate_T =
       Good
     | Bad
     | Timeout 
     | NCBUR   -- can be the expected result tho now not 'returned' by
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
    expected == actual -- FIXME[F1]: must relax!
  
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
    expected == actual -- FIXME[F1]: must relax!
  

---- tool: totext ----

totext_T :: Tool
totext_T =
  T { t_name          = "totext"
    , t_cmd_exec      = "pdf-hs-driver"
    , t_cmd_mkArgs    = (\f->["-t", f])
    , t_timeoutInSecs = timeoutInSecs
    , t_proj          = proj
    , t_cmp           = cmp
    }

  where
  timeoutInSecs = 3*60
    
  proj :: IO String -> IO String -> IO MetaData-> IO String
  proj getStdOut _getErr _getMeta =
    unlines . filter isText . lines <$> getStdOut
    where
    isText []                           = False   
    isText s | "INFO - " `isPrefixOf` s = False
             | otherwise                = True

  cmp expected actual =
    expected == actual  -- FIXME[F1]: must relax!
    -- note that we do unicode 'ff' while pdftotext does 2 'f' chars
    
