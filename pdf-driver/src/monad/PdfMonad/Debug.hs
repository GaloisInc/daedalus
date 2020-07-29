{-# Language GeneralizedNewtypeDeriving, BlockArguments, ImplicitParams #-}
{-# Language ConstraintKinds, RankNTypes #-}
module PdfMonad.Debug (Parser, runParser, DbgMode, pdfMain) where

import Data.ByteString(ByteString)
import Data.Foldable(toList)
import Data.Set(Set)
import qualified Data.Set as Set
import Control.Monad(when,liftM,ap)
import Text.Read(readMaybe)
import System.IO(hFlush, stdout)
import Data.IORef(IORef, newIORef, readIORef, modifyIORef',writeIORef)

import PdfMonad.Transformer as T
import qualified RTS.ParserT as RTS
import qualified RTS.WithTreeT as RTS
import RTS.Input

type DbgMode =
  ( ?breakOnByte :: IORef (Set Int)
  , ?treeRef     :: IORef RTS.Trie
  )

pdfMain :: (DbgMode => IO ()) -> IO ()
pdfMain i =
  do x <- newIORef Set.empty
     let ?breakOnByte = x

     t <- newIORef RTS.emptyTrie
     let ?treeRef = t
     i
     tr <- readIORef t
     putStrLn $ unlines $ RTS.showTrie tr

newtype Parser a = P (PdfT (RTS.WithTreeT (RTS.ParserT DbgM)) a)
  deriving (Functor,Applicative,Monad)

newtype DbgM a   = Dbg { runDbgM :: DbgMode => IO a }

getBreaks :: DbgM [Int]
getBreaks = Dbg do toList <$> readIORef ?breakOnByte

setBreak :: Int -> DbgM ()
setBreak n = Dbg do modifyIORef' ?breakOnByte (Set.insert n)

delBreak :: Int -> DbgM ()
delBreak n = Dbg do modifyIORef' ?breakOnByte (Set.delete n)

hasBreak :: Int -> DbgM Bool
hasBreak n = Dbg do Set.member n <$> readIORef ?breakOnByte

io :: IO a -> DbgM a
io m = Dbg m

dbg :: DbgM a -> Parser a
dbg m = P $ doM $ RTS.liftWithTree $ RTS.inBase m


instance Functor DbgM where
  fmap = liftM

instance Applicative DbgM where
  pure a = Dbg do pure a
  (<*>)  = ap

instance Monad DbgM where
  Dbg m >>= f = Dbg do a <- m
                       let Dbg m1 = f a
                       m1



runParser :: DbgMode => ObjIndex -> Parser a -> Input -> IO (PdfResult a)
runParser objMap pars i =
  do let P m = pars -- shell "Start parser" >> pars
     tree <- readIORef ?treeRef
     res <- runDbgM (RTS.runParserT
                      (RTS.runWithTreeT tree (runPdfT i objMap m)) i)
     case res of
       NoResults err -> pure (ParseErr err)
       Results ans ->
         case toList ans of
           [(a,t)] -> do writeIORef ?treeRef t
                         pure (ParseOk a)
           xs      -> pure (ParseAmbig (map fst xs))
           -- XXX: we do nothing when there are multiple results.
           -- In practise, we might be quite interested in the parse trees
           -- if there is an ambiguite, so we should do something more sensible
           -- (i.e., merge the tries, indicating from which version is each
           -- interpretation).

instance BasicParser Parser where
  P m ||| P n     = P $ m ||| n
  P m <|| P n     = P $ m <|| n
  pFail s         = P $ pFail s
  pByte r         = do checkBreak
                       P $ pByte r
  pEnter l (P m)  = P $ pEnter l m
  pStack          = P $ pStack
  pPeek           = P $ pPeek
  pSetInput i     = P $ pSetInput i

  pOffset         = P $ pOffset
  pEnd r          = P $ pEnd r
  pMatch1 r v     = do checkBreak
                       P $ pMatch1 r v

  pErrorMode e (P m) = P $ pErrorMode e m

  {-# INLINE (|||)      #-}
  {-# INLINE (<||)      #-}
  {-# INLINE pFail      #-}
  {-# INLINE pByte      #-}
  {-# INLINE pEnter     #-}
  {-# INLINE pStack     #-}
  {-# INLINE pPeek      #-}
  {-# INLINE pSetInput  #-}
  {-# INLINE pOffset    #-}
  {-# INLINE pEnd       #-}
  {-# INLINE pMatch1    #-}
  {-# INLINE pErrorMode #-}

--------------------------------------------------------------------------------

checkBreak :: Parser ()
checkBreak =
  do i <- pPeek
     yes <- dbg $ hasBreak $ inputOffset i
     when yes (shell "At break point")

shell :: String -> Parser ()
shell name =
  do dbg $ io $ putStrLn name
     stack <- pStack
     inp   <- pPeek

     let getCommand =
           do io $ putStr "Daedalus> " >> hFlush stdout
              txt <- io getLine
              processCommand (words txt)


         showHelp = do io $ putStrLn help
                       getCommand

         processCommand toks =
           case toks of
             "continue" : _ -> pure ()

             "offset" : _ ->
                do io $ print (inputOffset inp)
                   getCommand

             "break" : more ->
               case more of
                 [] -> do bs <- getBreaks
                          io $ mapM_ print bs
                          getCommand

                 "add" : xs
                   | Just is <- mapM readMaybe xs ->
                      do mapM_ setBreak is
                         getCommand
                   | otherwise ->
                      do io $ putStrLn "Failed to parse byte offsets"
                         getCommand

                 "del" : xs
                   | Just is <- mapM readMaybe xs ->
                       do mapM_ delBreak is
                          getCommand
                   | otherwise ->
                      do io $ putStrLn "Failed to parse byte offsets"
                         getCommand

                 _ -> do io $ putStrLn "Unknown `break` sub-command"
                         io $ putStrLn "We know about: (none), `add`, `del`"
                         getCommand

             "stack" : _ ->
               do io $ mapM_ putStrLn stack
                  getCommand

             "help" : _ -> showHelp

             _ ->
               do io $ putStrLn "Unknown command"
                  showHelp

     dbg getCommand

  where

  help = unlines
    [ "Commands:"
    , "--------"
    , "continue             Exit shell and continue executing."
    , "break                See a list of the current break points."
    , "break add OFFSET1+   Add break points at the give bytes."
    , "break del OFFSET1+   Remove break points from the given bytes."
    , "stack                See the current call stack."
    , "offset               See the current offset in the input."
    ]
