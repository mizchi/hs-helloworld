{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Lib (FetchError (..), Todo, fetchTodo) -- Import FetchError type
import qualified Network.HTTP.Req as Req
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Usage: helloworld-exe <URL>"
      exitFailure
    (url : _) -> do
      putStrLn $ "Fetching Todo from URL: " ++ url
      result <- Req.runReq Req.defaultHttpConfig $ fetchTodo url
      case result of
        Right todo -> do
          putStrLn "Successfully fetched Todo:"
          print (todo :: Todo)
        Left fetchError -> do
          putStrLn "Failed to fetch Todo:"
          case fetchError of
            InvalidUrlFormat urlStr ->
              putStrLn $ "Invalid URL format: " ++ urlStr
            NetworkError errMsg ->
              putStrLn $ "Network error: " ++ errMsg
            HttpError errMsg ->
              putStrLn $ "HTTP error: " ++ errMsg
            ParseError errMsg ->
              putStrLn $ "JSON parsing error: " ++ errMsg
          exitFailure
