module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)

processArgs :: [String] -> IO ()
processArgs [] = do
  putStrLn "Usage: cabal run examples -- <command>"
  exitFailure
processArgs (cmd : _) = case cmd of
  "monads" -> runMonadsExamples
  _ -> do
    putStrLn $ "Unknown command: " ++ cmd
    processArgs []

-- Maybe
runMonadsExamples :: IO ()
runMonadsExamples = do
  putStrLn "WIP Running Monad examples..."

-- | メイン関数
main :: IO ()
main = do
  putStrLn "======================================"
  args <- getArgs
  processArgs args
