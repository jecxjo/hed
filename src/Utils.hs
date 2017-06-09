{-# LANGUAGE OverloadedStrings #-}
module Utils ( justOr, printTerm ) where

import Data.Char (toLower)
import Data.Text (unpack, unlines, Text(..))
import Data.Vector (slice, length, null, drop, toList, Vector(..))
import System.Console.Terminal.Size
import System.IO
-- import System.IO (hSetBuffering, hGetBuffering, stdin, stdout, hFlush, BufferMode(..))
import Prelude hiding (length, unlines, drop, null)

justOr :: a -> Maybe a -> a
justOr _ (Just x) = x
justOr fault Nothing = fault

printTerm :: Vector Text -> IO ()
printTerm vec = do
    bufMode <- hGetBuffering stdin
    hSetBuffering stdin (BlockBuffering $ Just 1)
    if null vec
    then do
      hSetBuffering stdin bufMode
      return ()
    else do
      printVec 0
      hSetBuffering stdin bufMode
  where
    vecSize = length vec
    getHeight (Just w) = height w
    getHeight Nothing = 22
    printVec idx = do
      termSize <- size
      let termSize' = getHeight termSize
      if vecSize - idx > termSize'
      then do
        (putStrLn . unpack . unlines . toList) $ slice idx (termSize' - 2) vec
        putStr "--- (n)ext / (p)revious / (q)uit: "
        hFlush stdout
        x <- getLine
        let cmd = if x == "" then 'n' else toLower $ head x
        case cmd of
          'n' -> printVec (idx + termSize' - 2)
          'p' -> printVec (max 0 (idx - termSize' + 2))
          'q' -> return ()
          _   -> printVec idx
      else do
        (putStrLn . unpack . unlines . toList) $ drop idx vec
