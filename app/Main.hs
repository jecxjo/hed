module Main where

import System.Environment (getArgs)

import Runner (runEditor)

usage :: IO ()
usage = do
  putStrLn "Usage: hed <filename>"

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runEditor Nothing
    1 -> runEditor (Just $ head args)
    _ -> usage
