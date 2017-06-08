{-# LANGUAGE OverloadedStrings #-}

module Commands
  ( RelativeDirection(..)
  , Position(..)
  , Range(..)
  , Args(..)
  , CommandName(..)
  , StateInfo(..)
  , CommandState(..)
  ) where

import Data.List (find)
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Monad.State
import System.IO (hFlush, stdout)

data RelativeDirection = Plus | Minus
  deriving (Show, Eq)

data Position = NoPosition -- For second when a single pos cmd
              | CurrentPosition -- . position
              | EndOfDoc -- $
              | AbsolutePosition Int -- 1,2
              | RelativePosition RelativeDirection Int -- +1,-1
              | NextLineMatching T.Text
              | PreviousLineMatching T.Text
              | MarkLocation Char
              deriving (Show, Eq)

data Range = NoRange
           | SingleRange Position
           | DoubleRange Position Position
           deriving (Show, Eq)

type CommandName = T.Text

data Args = NoArgs
          | StringArg T.Text
          | RangeArg Range
          | RegExArg T.Text T.Text
          | NumberArg Int
          deriving (Show, Eq)


type CommandState = StateT StateInfo IO ()

data StateInfo = StateInfo
  { contents :: V.Vector T.Text
  , fileName :: FilePath
  , dirty :: Bool
  , dirtyWarning :: Bool
  , position :: Int
  , prompt :: T.Text
  , showPrompt :: Bool
  , lastError :: T.Text
  , clipboard :: V.Vector T.Text
  , displayHelp :: Bool
  , marks :: V.Vector (Char, Int)
  , lastSearch :: T.Text
  , lastReplace :: T.Text
  , lastScroll :: Int
  -- Temporary Data
  , tBuffer :: V.Vector T.Text
  , tFromStart :: Int
  , tFromEnd :: Int
  , tToStart :: Int
  , tToEnd :: Int
  }

instance Show StateInfo where
  show st =
    "----\n" ++
    "contents: " ++ (show $ contents st) ++ "\n" ++
    "fileName: " ++ (show $ fileName st) ++ "\n" ++
    "dirty: " ++ (show $ dirty st) ++ "\n" ++
    "dirtyWarning: " ++ (show $ dirtyWarning st) ++ "\n" ++
    "position: " ++ (show $ position st) ++ "\n" ++
    "prompt: " ++ (show $ prompt st) ++ "\n" ++
    "showPrompt: " ++ (show $ showPrompt st) ++ "\n" ++
    "lastError: " ++ (show $ lastError st) ++ "\n" ++
    "clipboard: " ++ (show $ clipboard st) ++ "\n" ++
    "displayHelp: " ++ (show $ displayHelp st) ++ "\n" ++
    "marks: " ++ (show $ marks st) ++ "\n" ++
    "lastSearch: " ++ (show $ lastSearch st) ++ "\n" ++
    "tBuffer: " ++ (show $ tBuffer st) ++ "\n" ++
    "tFromStart: " ++ (show $ tFromStart st) ++ "\n" ++
    "tFromEnd: " ++ (show $ tFromEnd st) ++ "\n" ++
    "tToStart: " ++ (show $ tToStart st) ++ "\n" ++
    "tToEnd: " ++ (show $ tToEnd st) ++ "\n" ++
    "----\n"
