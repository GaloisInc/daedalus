{-# LANGUAGE PartialTypeSignatures, NamedFieldPuns #-}

-- system:
import           Control.Monad
import           Data.Char
import           Data.Maybe
import           System.Exit

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

tools = [validate_T]

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
  timeoutInSecs = 5 -- 5*60 -- FIXME
    
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

data Result = Good | Bad | Timeout
              deriving (Eq, Read, Show)  

---- code --------------------------------------------------------------------

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=".shake"} $
  do
  runTest "validatePDF" "misc"
  -- FIXME: nab from commandline
  
-- | runTest - one tool, one directory of inputs, one 'summary'
runTest :: FilePath -> FilePath -> Rules ()
runTest toolNm corpDir =
  do
  let srcDir     = "corp" </> corpDir
      testDir    = concat ["test","--",toolNm,"--",corpDir]
      resultDir  = testDir </> "results"
      expctdDir  = testDir </> "expctd"
      summaryF   = testDir </> "test-summary"
      variancesF = testDir </> "variances.filelist"
        
        
  T{t_name,t_cmd_exec,t_cmd_mkArgs,t_timeoutInSecs,t_proj,t_cmp} <-
    case [ t | t <- tools, t_name t == toolNm ] of
      [x] -> return x
      []  -> fail $ "not valid tool name: " ++ toolNm
      _   -> error "in 'tools'"
    
  action $ do       
    do e <- doesDirectoryExist srcDir
       unless e $ fail $ "corpora directory does not exist: " ++ srcDir
    do e <- doesDirectoryExist testDir
       unless e $ fail $ "test directory does not exist: " ++ testDir
    
  want [summaryF]

  phony "clean" $
    do
    putInfo "Cleaning files"
    removeFilesAfter resultDir ["//*"]
    removeFilesAfter "." [summaryF]
    
  map (resultDir </>) ["*.stdout","*.stderr","*.meta"]
     &%> \[outF,errF,metaF] ->
    do
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
    writeFile' resultF resultC
      
  summaryF %> \summaryF' ->
    do
    -- needed source files:
    let firstCharNonWhite []    = False
        firstCharNonWhite (c:_) = not (isSpace c)
        
    need [variancesF]
    varianceFiles <- filter firstCharNonWhite . lines
                     <$> readFile' variancesF
    exps <- getDirectoryFiles expctdDir ["*.result-expctd"]
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
                     return $ case (eqv,variance) of
                       (False,False) -> Just (baseF, NE_NoVariance)
                       (True ,True ) -> Just (baseF, EQ_Variance)
                       _             -> Nothing
                       
    let rs' = catMaybes rs
        report = unlines $
          [ show(length rs) ++ " files"
          , show(length rs') ++ " problem(s):"
          ]
          ++ (let fs = [ f | (f, NE_NoVariance) <- rs'] in
              if null fs then
               []
              else
                "inputs where results not equal but no variance specified:"
                : map ("  "++) fs)
          ++ (let fs = [ f | (f, EQ_Variance) <- rs'] in
              if null fs then
                []
              else
                "inputs where variance specified but results are equal:"
                : map ("  "++) fs)
          
    putInfo report
    writeFile' summaryF' report
{-
ppErrorType = \case  
 Eq_Variance   -> "Variance When Equal"
 NE_NoVariance -> "Not Equal, no Variance"
-}



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



