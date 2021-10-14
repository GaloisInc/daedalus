{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

-- base

import qualified Data.ByteString.Char8 as B8 
import qualified Data.ByteString       as BS
import           Data.List
import           System.Environment
import           System.Exit

-- pkg process:
import System.Process

-- local:
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
renderTest (Test _nm (h1,h2,ds,di) ty edits ctgy notes) =
  applyEdits edits
             (render_PDF_DOM ty (h1, commentsAndSuch <> h2, ds, di))

  where
  commentsAndSuch =
      BS.intercalate "\n"
    $ (   map (\s->"% NOTE: " <> fillTo100 s) notes
       ++ map (\s-> "% " <> B8.pack s) (ppCtgy ctgy)
       ++ [""])

  fillTo100 s = B8.take 100 (s <> " " <> B8.replicate 100 '%')
    
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
  -- these tests all based on the same "DOM" pdf1:
  [ Test "1_valid_xref_trad" pdf1 RT_XRef_Trad E_None Good
      ["no compressed objects, traditional xref table"]
      
  , Test "1_invalid_xref_trad-a" pdf1 RT_XRef_Trad
      (E_Shell "sed" ["78s/000732/000733/"])
      (Probs [(Error    , "object id (11) mismatch")
             ,(NotStrict, "cavity with \"1\" in it, before object 11")
             ])
      ["no compressed objects, traditional xref table; object 11 pointing +1"]
      
  , Test "1_invalid_xref_trad-b" pdf1 RT_XRef_Trad
      (E_Shell "sed" ["61s/11 0 obj/23 0 obj/"])
      (Probs [(Error , "object id (11 /= 23) mismatch")
             ])
      ["no compressed objects, traditional xref table; object 11 xref pointing to object 23"]
      
  , Test "1_valid_xref_strm" pdf1 RT_XRef_Trad E_None  Good
      ["no compressed objects, xref is stream"]

  -- this is based on a DOM that uses "compressed objects":
  , Test "2_valid_xref_strm" pdf2 RT_XRef_Strm E_None Good
      ["compressed objects, thus xref is stream"]
      
  -- this is based on a DOM that uses "compressed objects":
  , Test "2_valid_xref_strm_indirects" pdf2 RT_XRef_Strm
      (E_Shell "sed" [intercalate ";" ["s#/N 5#/N 1 0 R#"           -- using 4 extra chars
                                      ,"s#/First 28#/First 2 0 R#"  -- using 3 extra chars
                                      ,"s/UNUSED 1234567/UNUSED /" -- remove 7 chars
                                      ]])
      -- E_None
      Good
      ["compressed objects, thus xref is stream, using indirects for N and First"]
      
  ]

