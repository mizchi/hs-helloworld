{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Lib
  ( FetchError (..),
    Todo,
    concurrentFetch2,
    fetchTodo,
    fetchWithTimeout,
    waitFetchResult,
  )
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- | コマンドライン引数の処理
processArgs :: [String] -> IO ()
processArgs [] = do
  putStrLn "Usage: helloworld-exe <command> [args]"
  putStrLn "Commands:"
  putStrLn "  fetch <URL>              - 同期的にTodoを取得"
  putStrLn "  async <URL>              - 非同期でTodoを取得"
  putStrLn "  concurrent <URL1> <URL2> - 2つのURLから並行してTodoを取得"
  putStrLn "  timeout <microseconds> <URL> - タイムアウト付きでTodoを取得"
  exitFailure
processArgs (cmd : args) = case cmd of
  "fetch" -> handleFetch args
  "async" -> handleAsync args
  "concurrent" -> handleConcurrent args
  "timeout" -> handleTimeout args
  _ -> do
    putStrLn $ "Unknown command: " ++ cmd
    processArgs []

-- | 同期的にTodoを取得
handleFetch :: [String] -> IO ()
handleFetch [] = do
  putStrLn "Usage: helloworld-exe fetch <URL>"
  exitFailure
handleFetch (url : _) = do
  putStrLn $ "Fetching Todo from URL: " ++ url
  asyncResult <- fetchTodo url
  result <- waitFetchResult asyncResult
  handleFetchResult result

-- | 非同期でTodoを取得
handleAsync :: [String] -> IO ()
handleAsync [] = do
  putStrLn "Usage: helloworld-exe async <URL>"
  exitFailure
handleAsync (url : _) = do
  putStrLn $ "Starting async fetch from URL: " ++ url
  -- 非同期処理を開始
  asyncResult <- fetchTodo url
  putStrLn "Async fetch started. Doing other work..."

  -- 他の処理を実行（例として1秒待機）
  threadDelay 1000000
  putStrLn "Other work completed. Waiting for fetch result..."

  -- 非同期処理の結果を待つ
  result <- waitFetchResult asyncResult
  handleFetchResult result

-- | 複数のURLから並行してTodoを取得
handleConcurrent :: [String] -> IO ()
handleConcurrent args = case args of
  [url1, url2] -> do
    putStrLn $ "Fetching concurrently from: " ++ url1 ++ " and " ++ url2
    (result1, result2) <- concurrentFetch2 url1 url2

    putStrLn "\nResults from first URL:"
    handleFetchResult result1

    putStrLn "\nResults from second URL:"
    handleFetchResult result2
  _ -> do
    putStrLn "Usage: helloworld-exe concurrent <URL1> <URL2>"
    exitFailure

-- | タイムアウト付きでTodoを取得
handleTimeout :: [String] -> IO ()
handleTimeout args = case args of
  [timeoutStr, url] -> case reads timeoutStr of
    [(timeout, "")] -> do
      putStrLn $ "Fetching with " ++ timeoutStr ++ " microseconds timeout from: " ++ url
      result <- fetchWithTimeout timeout url
      handleFetchResult result
    _ -> do
      putStrLn "Invalid timeout value. Must be an integer."
      exitFailure
  _ -> do
    putStrLn "Usage: helloworld-exe timeout <microseconds> <URL>"
    exitFailure

-- | 取得結果の処理
handleFetchResult :: Either FetchError Todo -> IO ()
handleFetchResult result = case result of
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

-- | メイン関数
main :: IO ()
main = do
  args <- getArgs
  processArgs args
