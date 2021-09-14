{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B8 
import qualified Data.ByteString       as BS

import System.Environment
import System.Exit

-- pkg process:
import System.Process

import DomGeneration
import DomRender
import DomExamples


---- types -------------------------------------------------------------------

data Test = Test { t_name     :: String 
                 , t_dom      :: PDF_DOM
                 , t_rtype    :: RenderType
                 , t_edits    :: Edits
                 , t_ctgy     :: Ctgy
                 , t_comments :: [BS]
                 }

data Edits = E_None | E_Shell String [String]
             deriving (Eq, Ord, Read, Show)

data Ctgy = Good                       -- ^ completely valid PDF
          | Probs [(MsgCtgy, String)]  -- ^ some forms of warnings/errors
          deriving (Eq, Ord, Read, Show)

data MsgCtgy = Error
             | Warn         -- distinguish exuberance?
             | NotStrict
             deriving (Eq, Ord, Read, Show)

---- functions ---------------------------------------------------------------

main =
  do
  as <- getArgs
  case as of
    [p] -> mapM_ (writeTest p) allTests
    _   -> usageError

usageError :: IO a
usageError =
  do
  pn <- getProgName
  putStrLn $ "Usage: " ++ pn ++ " directory-to-write-files"
  exitFailure

renderTest :: Test -> IO BS
renderTest (Test _nm (h1,h2,ds,di) ty edits ctgy cs) =
  applyEdits edits
             (render_PDF_DOM ty (h1, commentsAndSuch <> h2, ds, di))

  where
  commentsAndSuch =
      BS.intercalate "\n"
    $ (   map ("% NOTE: " <>) cs
       ++ map (\s-> "% " <> B8.pack s) (ppCtgy ctgy)
       ++ [""])
  
writeTest :: FilePath -> Test -> IO ()
writeTest dir t = renderTest t >>= B8.writeFile (dir ++"/" ++ t_name t ++ ".pdf")

outputTest :: Test -> IO ()
outputTest t = renderTest t >>= B8.putStrLn 

applyEdits :: Edits -> BS -> IO BS
applyEdits e bs =
  case e of
    E_None         -> return bs
    E_Shell f args -> B8.pack <$> readProcess f args (B8.unpack bs)

ppCtgy :: Ctgy -> [String]
ppCtgy = \case
           Good     -> ["CATEGORY: good PDF"]
           Probs ps -> "CATEGORY: discrepancies:"
                       : map ppLine ps
                       
ppLine (mc,s) = "  " ++ show mc ++ ": " ++ s

---- tests -------------------------------------------------------------------

allTests =
  [ Test "1_valid_xref_trad" pdf1 RT_XRef_Trad E_None Good
      ["no compressed objects, traditional xref table"]
      
  , Test "1_invalid_xref_trad" pdf1 RT_XRef_Trad
      (E_Shell "sed" ["78s/000732/000733/"])
      (Probs [(Error    , "object id (11) mismatch")
             ,(NotStrict, "cavity with \"1\" in it, before object 11")
             ])
      ["no compressed objects, traditional xref table; object 11 pointing +1"]
      
  , Test "1_valid_xref_strm" pdf1 RT_XRef_Trad E_None  Good
      ["no compressed objects, xref is stream"]
      
  , Test "2_valid_xref_strm" pdf2 RT_XRef_Strm E_None Good
      ["compressed objects, thus xref is stream"]
      
  ]

