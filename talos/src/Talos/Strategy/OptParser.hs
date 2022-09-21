{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Talos.Strategy.OptParser where

import           Control.Lens         (Setter', set)
import           Control.Monad.Except (Except, MonadError, runExcept, catchError, throwError)
import           Control.Monad.State  (MonadState, StateT, evalStateT, get,
                                       gets, put, state, modify)
import           Data.Char            (isAlpha, isSpace)
import           Data.Foldable        (find)
import           Data.List            (stripPrefix)
import Control.Monad (void)
import Control.Applicative (Alternative)

-- -----------------------------------------------------------------------------
-- Option Parsing for strategies

newtype Parser a = Parser { getParser :: StateT String (Except String) a }
  deriving (Functor, Applicative, Alternative, Monad, MonadError String, MonadState String)

data Opt t = forall a.  Opt
  { optName   :: String
  , optSetter :: Setter' t a
  , optParser :: Parser a
  }

nameP :: Parser String
nameP = state (span isNameChar)
  where
    isNameChar = (||) <$> isAlpha <*> ( (||) <$> (== '_') <*> (== '-'))

tokenP :: String -> Parser String
tokenP t = do
  str <- get
  case stripPrefix t str of
    Nothing -> throwError ("Expecting '" ++ t ++ "', saw '" ++ str ++ "'")
    Just rest -> put rest
  pure t

doneP :: Parser Bool
doneP = gets null

skipSpaces :: Parser ()
skipSpaces = modify (dropWhile isSpace)

runParser :: Parser a -> String -> Either String a
runParser p input = runExcept (evalStateT (getParser p) input)

parseOpts :: [Opt t] -> t {- default -} -> Parser t
parseOpts opts = go
  where
    go t = do
      skipSpaces
      name <- nameP
      if null name
        then pure t
        else do
        Opt { .. } <- case find ((==) name . optName) opts of
                        Nothing -> throwError ("Unknown option '" ++ name ++ "'")
                        Just o  -> pure o
        v <- optParser `catchError` \e -> throwError ("When parsing option '" ++ name ++ "': " ++ e)
        go (set optSetter v t)
  
-- ----------------------------------------------------------------------------------------
-- Parsers

readP :: Read a => Parser a
readP = do
  s <- get
  (r, s') <- case reads s of
               []  -> throwError "No match"
               (r : _) -> pure r -- take the first result
  put s'
  pure r

intP :: Parser Int
intP = readP 

floatP :: Parser Float
floatP = readP 

-- ----------------------------------------------------------------------------------------
-- Options/flags

option :: String -> Setter' t a -> Parser a -> Opt t
option n s p = Opt n s p'
  where
    p' = do
      skipSpaces
      void $ tokenP "="
      skipSpaces
      p

-- Sets the flag
flag :: String -> Setter' t Bool -> Opt t
flag n s = Opt n s (pure True)

