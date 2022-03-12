{-# LANGUAGE PartialTypeSignatures, NamedFieldPuns #-}
module Main where

-- system:
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           GHC.Generics (Generic)
import           System.Exit

-- shake pkg:
import qualified Development.Shake as Shake
import           Development.Shake         hiding (Timeout)
import           Development.Shake.FilePath
import           Development.Shake.Util

-- local:
import           Tools
import           Types
import           Util

---- code --------------------------------------------------------------------

main :: IO ()
main =
  shakeArgsWith
    shakeOptions{shakeFiles=".shake",shakeProgress=progressSimple}
    options
    (\flags ts-> do
                 opts <- processFlags flags
                 main' opts ts
    )

main' :: Options -> [FilePath] -> IO (Maybe (Rules ()))
main' opts targets =
  do
  let cmd_exec = t_cmd_exec (tool opts)
  (Exit ecode, StdoutTrim toolPath) <-
    cmd "which" [cmd_exec]
  unless (ecode == ExitSuccess) $
    fail $ "tool is not in path: '" ++ cmd_exec ++ "'"

  let quote s = "'" ++ s ++ "'"
  putStrLn $ unwords [ "Results of testset with tool"
                     , quote (toolName opts)
                     , "and corpora"
                     , quote (corpName opts) ++ ":"
                     ]
  let runTest' = runTest opts toolPath
  return
    $ Just
    $ if null targets then
        runTest'
      else
        want targets >> withoutActions runTest'
        
-- | runTest - one tool, one directory of inputs, one 'summary'
runTest :: Options -> FilePath -> Rules ()
runTest opts toolPath =
  do
  let T{t_name,t_cmd_exec,t_cmd_mkArgs,t_timeoutInSecs,t_proj,t_cmp} =
        tool opts
      timeoutInSecs = case timeOut opts of
                        Just t  -> t
                        Nothing -> t_timeoutInSecs
      srcDir     = "corpora" </> (corpName opts)
      testDir    = concat ["test","_",t_name,"_",corpName opts]
      resultDir  = testDir </> "results"
      expectedDir  = testDir </> "expected"
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
      command [ Shake.Timeout (fromIntegral timeoutInSecs)
              , WithStdout True
              , WithStderr True
              , EchoStderr False
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
      (case resultC of
         [] -> "\n"
         _  -> resultC ++ if last resultC /= '\n' then ['\n'] else []
      )
      
  resultDir</>"*.diff" %> \diffF ->
    do
    let baseF     = takeBaseName diffF
        actualF   = resultDir   </> baseF <.> "result-actual"
        expectedF = expectedDir </> baseF <.> "result-expected"
    need [expectedF,actualF]
    cResult <- liftIO $ cmpFileContents t_cmp expectedF actualF
    writeFile' diffF $
      show cResult
    {-
      case eqv of
        Right () -> "equivalent"
        Left ss  -> "not-equivalent: " ++ unlines ss
    -}
      
  summaryF %> \summaryF' ->
    do
    let firstCharNonWhite []    = False
        firstCharNonWhite (c:_) = not (isSpace c)
        
    need [variancesF]
    varianceFiles <- filter firstCharNonWhite . lines
                     <$> readFile' variancesF

    -- we work on the '*.result-expected' files:
    exps <- getDirectoryFiles "" [expectedDir </> "*.result-expected"]
    let basenames = map
                      (takeFileName . sfStripExtension ".result-expected")
                      exps

    -- need the diff file for each result-expected file:
    need $ map
             (\b->resultDir </> b <.> "diff")
             basenames

    -- compute the problems / unexpected variances:
    ps <- catMaybes <$> (
          forM basenames
              $ \baseF-> do
                         let diffF = resultDir </> baseF <.> "diff"
                             variance  = baseF `elem` varianceFiles
                         diff <- read <$> readFile' diffF
                         return $ case (diff,variance) of
                           (False,False) -> Just (baseF, NE_NoVariance)
                           (True ,True ) -> Just (baseF, EQ_Variance)
                           _             -> Nothing
          )
          
    -- generate summary report:
    let report = unlines $
          [ show(length basenames) ++ " files"
          , case length ps of
              0 -> "0 unexpected variances."
              n -> show n ++ " unexpected variance(s):"
          ]
          ++ (let fs = [ f | (f, NE_NoVariance) <- ps] in
              if null fs then
               []
              else
                " Files where result =/= expected but no variance specified:"
                : map ("  "++) fs)
          ++ (let fs = [ f | (f, EQ_Variance) <- ps] in
              if null fs then
                []
              else
                " Files where result == expected but a variance is specified:"
                : map ("  "++) fs)
    writeFile' summaryF' report

