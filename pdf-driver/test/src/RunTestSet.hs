{-# LANGUAGE PartialTypeSignatures, NamedFieldPuns #-}
module Main where

-- system:
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           GHC.Generics (Generic)
import           System.Exit
import           System.Console.GetOpt

-- shake pkg:
import qualified Development.Shake as Shake
import           Development.Shake         hiding (Timeout)
import           Development.Shake.FilePath
import           Development.Shake.Util



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

    , t_cmp           :: String -> String -> Bool -- or, expand
    }

  -- FIXME: name things as an 'isvalid' (not 'result') projection
  -- and abstract over projections (e.g., have multiple, named projs)

  
data ErrorType = EQ_Variance
               | NE_NoVariance
               deriving (Eq, Read, Show)

data MetaData = MetaData { exitCode :: ExitCode
                         , runtime  :: Int
                         }
                deriving (Eq,Show,Read)

---- tools -------------------------------------------------------------------

tools :: [Tool]
tools = [validate_T]

toolnames :: [String]
toolnames = map t_name tools
  
toolnameRegEx :: String
toolnameRegEx = intercalate "|" toolnames
  
validate_T :: Tool
validate_T =
  T { t_name          = "validatePDF"
    , t_cmd_exec      = "pdf-hs-driver"
    , t_cmd_mkArgs    = (\f->[f])
    , t_timeoutInSecs = timeoutInSecs
    , t_proj          = validate_proj
    , t_cmp           = (==)
    }

  where
  timeoutInSecs = 3*60  -- FIXME: be able to specify on command line??
    
  validate_proj :: IO String -> IO String -> IO MetaData-> IO String
  validate_proj _getOut _getErr getMeta =
    do
    metaD <- getMeta
    let r = if runtime metaD >= timeoutInSecs*1000 then
              Timeout
            else if exitCode metaD == ExitSuccess then
              Good
            else
              Bad
    return (show r)

data ValidateResult =
       Good
     | Bad
     | Timeout 
     | NCBUR   -- can be the expected result tho now not 'returned' by
     deriving (Eq, Read, Show)  


---- code --------------------------------------------------------------------

data Flags = F_ToolName String
           | F_CorporaName String
           deriving (Eq,Show,Ord)

options =
  [ Option [] ["tool"]    (ReqArg mkToolName    toolnameRegEx) "tool name"
  , Option [] ["corpora"] (ReqArg mkCorporaName "CORP"       ) "corpora name"
  ]
  where
  mkCorporaName s = Right (F_CorporaName s)
  mkToolName s
    | s `elem` toolnames = Right (F_ToolName s)
    | otherwise          = Left $ "toolname must be '" ++ toolnameRegEx ++ "'"
  
main :: IO ()
main =
  shakeArgsWith
    shakeOptions{shakeFiles=".shake"}
    options
    main'
  
main' :: [Flags] -> [FilePath] -> IO (Maybe (Rules ()))
main' flags targets =
  do
  (toolName,corpName) <-
    case sort flags of
      [F_ToolName tn, F_CorporaName cn] -> return (tn,cn)
      _                                 -> quit msg
        where
        msg = "must specify both --tool <tname> and --corpora <cname>"
  t <- case [ t | t <- tools, t_name t == toolName ] of
         [x] -> return x
         []  -> fail $ "not a valid tool name: " ++ toolName
         _   -> error "in 'tools/runTest'"

  (Exit ExitSuccess, StdoutTrim toolPath) <-
    cmd "which" [t_cmd_exec t]
    -- NB: if 'which' fails, program fails.

  let quote s = "'" ++ s ++ "'"
  putStrLn $ unwords [ "Results of testset with tool"
                     , quote toolName
                     , "and corpora"
                     , quote corpName ++ ":"
                     ]
  let runTest' = runTest t toolPath corpName flags
  return
    $ Just
    $ if null targets then
        runTest'
      else
        want targets >> withoutActions runTest'
        
-- | runTest - one tool, one directory of inputs, one 'summary'
runTest :: Tool -> FilePath -> String -> [Flags] -> Rules ()
runTest t toolPath corpName flags =
  do
  let T{t_name,t_cmd_exec,t_cmd_mkArgs,t_timeoutInSecs,t_proj,t_cmp} = t
  let srcDir     = "corpora" </> corpName
      testDir    = concat ["test","_",t_name,"_",corpName]
      resultDir  = testDir </> "results"
      expctdDir  = testDir </> "expctd"
      summaryF   = testDir </> "test-summary"
      variancesF = testDir </> "variances.filelist"

  action $ do
    do e <- doesDirectoryExist srcDir
       unless e $ fail $ "corpora directory does not exist: " ++ srcDir
    do e <- doesDirectoryExist testDir
       unless e $ fail $ "test directory does not exist: " ++ testDir
    need [summaryF]
    liftIO $ readFile summaryF >>= putStrLn 
             
  phony "clean" $
    do
    putInfo "Cleaning files"
    removeFilesAfter resultDir ["//*"]
    removeFilesAfter testDir [summaryF]
    
  map (resultDir </>) ["*.stdout","*.stderr","*.meta"]
     &%> \[outF,errF,metaF] ->
    do
    need [toolPath]
    let srcBase = sfStripExtension ".stdout" outF
        src     = replaceDirectory srcBase srcDir
    need [src]
    (Exit code, CmdTime t) <-
      command [ Shake.Timeout (fromIntegral t_timeoutInSecs)
              , WithStdout True
              , WithStderr True
              , EchoStderr True
              , FileStdout outF
              , FileStderr errF
              ]
              t_cmd_exec (t_cmd_mkArgs src)
    writeFile' metaF
       $ triviallyFormat
       $ show
       $ MetaData code (timeInMs t)

  resultDir</>"*.result-actual" %> \resultF ->
    do
    let outF  = resultF -<.> ".stdout"
        errF  = resultF -<.> ".stderr"
        metaF = resultF -<.> ".meta"
    need [outF,errF,metaF]
    resultC <- liftIO
             $ t_proj (readFile outF)
                      (readFile errF)
                      ((\s-> read s :: MetaData) <$> readFile metaF)
    writeFile'
      resultF
      (resultC ++ if last resultC /= '\n' then ['\n'] else [])
      
  summaryF %> \summaryF' ->
    do
    -- needed source files:
    let firstCharNonWhite []    = False
        firstCharNonWhite (c:_) = not (isSpace c)
        
    need [variancesF]
    varianceFiles <- filter firstCharNonWhite . lines
                     <$> readFile' variancesF
    exps <- getDirectoryFiles "" [expctdDir </> "*.result-expctd"]
    -- putInfo $ "exps: " ++ show exps
    need exps

    -- needed generated files:
    let basenames = map
                      (takeFileName . sfStripExtension ".result-expctd")
                      exps
        baseToActual b = resultDir </> b <.> "result-actual"
    need $ map baseToActual basenames
    
    rs <- forM basenames
          $ \baseF-> do
                     let expctdF  = expctdDir </> baseF <.> "result-expctd"
                         actualF  = resultDir </> baseF <.> "result-actual"
                         variance = baseF `elem` varianceFiles
                     eqv <- liftIO $ cmpFileContents t_cmp expctdF actualF
                     return $! case (eqv,variance) of
                       (False,False) -> Just (baseF, NE_NoVariance)
                       (True ,True ) -> Just (baseF, EQ_Variance)
                       _             -> Nothing
                       
                       -- N.B. the $! : this fixes a resource leak where
                       -- we have all the files open at once.
                       
    let rs' = catMaybes rs
        report = unlines $
          [ show(length rs) ++ " files"
          , case length rs' of
              0 -> "0 problems."
              n -> show n ++ " problem(s):"
          ]
          ++ (let fs = [ f | (f, NE_NoVariance) <- rs'] in
              if null fs then
               []
              else
                " Files where result =/= expctd but no variance specified:"
                : map ("  "++) fs)
          ++ (let fs = [ f | (f, EQ_Variance) <- rs'] in
              if null fs then
                []
              else
                " Files where result == expctd but a variance is specified:"
                : map ("  "++) fs)
          
    writeFile' summaryF' report


---- utilities ---------------------------------------------------------------

cmpFileContents :: (String -> String -> Bool)
                -> FilePath -> FilePath -> IO Bool
cmpFileContents eqv fa fb =
  do
  ca <- readFile fa
  cb <- readFile fb
  return (eqv ca cb)


triviallyFormat []     = ""
triviallyFormat (c:cs) =
  if c `elem` "{,}" then
    "\n  " ++ [c] ++ triviallyFormat cs
  else
    c : triviallyFormat cs
 
timeInMs x = round(x * 1000)

sfStripExtension :: String -> FilePath -> FilePath
sfStripExtension ext fp = case stripExtension ext fp of
                            Just fp' -> fp'
                            Nothing  -> error "sfStripExtension"

quit :: String -> IO a
quit msg =
  do putStrLn msg
     exitFailure
     

