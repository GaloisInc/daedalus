{-# LANGUAGE LambdaCase #-}

-- | dead simple logging mechanism/convention
module Logger
  ( logInfo
  , logWarn
  , logError
  , quit
  , logWarnS
  , logErrorS
  , logWarnIfFail
  , logErrorIfFail
  , logWarnIndent
  , logWarnSIndent
  )    
where

import System.Exit(exitFailure)

import Daedalus.Panic 
import Possibly

-- FIXME[F2] TODO: improve/extend?
--           - allow control by a verbose flag or the like.

logInfo :: String -> IO ()
logInfo s = putStrLn $ "INFO: " ++ s

logWarn :: String -> IO ()
logWarn s = putStrLn $ "WARN: " ++ s

logError :: String -> IO ()
logError s = quit ("ERROR: " ++ s)

quit :: String -> IO a
quit msg = do putStrLn msg
              exitFailure

logWarnS :: [String] -> IO ()
logWarnS []     = panic "logWarnS with empty message" []
logWarnS (s:ss) = mapM_ putStrLn $ ("WARN: "++s) : map ("  "++) ss
  
logErrorS :: [String] -> IO a
logErrorS []     = panic "logErrorS with empty message" []
logErrorS (s:ss) = do
                   mapM_ putStrLn $ ("ERROR: "++s) : map ("  "++) ss
                   exitFailure
                   
logWarnIfFail :: Possibly a -> IO ()
logWarnIfFail = 
 \case
   Right _     -> return ()
   Left ss     -> logWarnS ss

logErrorIfFail :: Possibly a -> IO a
logErrorIfFail = 
 \case
   Right a     -> return a
   Left ss     -> logErrorS ss

--- indenting versions

indent :: Int -> [Char] -> [Char]
indent i s = replicate (2*i) ' ' ++ s

logWarnIndent :: Int -> String -> IO ()
logWarnIndent i s = putStrLn $ indent i "WARN: " ++ s

logWarnSIndent :: Int -> [String] -> IO ()
logWarnSIndent _ []     = panic "logWarnS with empty message" []
logWarnSIndent i (s:ss) = mapM_ putStrLn $
                            (indent i "WARN: "++s) : map (indent i "  "++) ss
  
