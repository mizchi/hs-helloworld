{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Lib (fetchTodo, Todo)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Network.HTTP.Req as Req

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Usage: helloworld-exe <URL>"
      exitFailure
    (url:_) -> do
      putStrLn $ "Fetching Todo from URL: " ++ url
      maybeTodo <- Req.runReq Req.defaultHttpConfig $ fetchTodo url
      case maybeTodo of
        Just todo -> do
          putStrLn "Successfully fetched Todo:"
          print (todo :: Todo)
        Nothing -> do
          putStrLn "Failed to fetch Todo."
          exitFailure
