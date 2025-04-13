{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( Todo(..)
    , fetchTodo
    , quicksort
    ) where

import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Req as Req
import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.List (intercalate)

data Todo = Todo
  { userId :: Int
  , id :: Int
  , title :: Text
  , completed :: Bool
  } deriving (Generic, Show, Eq)
instance Aeson.FromJSON Todo

splitString :: String -> String -> [String]
splitString _ [] = []
splitString delim str =
  case findDelim delim str of
    Nothing -> [str]
    Just i  ->
      let (before, after) = splitAt i str
          afterDelim = drop (length delim) after
      in before : splitString delim afterDelim

findDelim :: String -> String -> Maybe Int
findDelim _ [] = Nothing
findDelim delim str@(_:xs)
  | take (length delim) str == delim = Just 0
  | otherwise = case findDelim delim xs of
      Nothing -> Nothing
      Just i  -> Just (i + 1)

parseUrl :: String -> Maybe (Bool, String, String, Int, String)
parseUrl url = do
  let parts = splitString "://" url
  if length parts /= 2
    then Nothing
    else do
      let scheme = parts !! 0
      let rest = parts !! 1

      -- Check if scheme is valid
      isHttps <- case scheme of
        "http" -> Just False
        "https" -> Just True
        _ -> Nothing

      -- Split the rest by "/" to get host and path
      let hostAndRest = splitString "/" rest
      if null hostAndRest
        then Nothing
        else do
          let hostPart = hostAndRest !! 0

          -- Split host by ":" to get host and port
          let hostAndPort = splitString ":" hostPart
          let host = hostAndPort !! 0

          -- Parse port or use default
          let port = if length hostAndPort > 1
              then fromMaybe (if isHttps then 443 else 80) (readMaybe (hostAndPort !! 1))
              else (if isHttps then 443 else 80)
          -- Join the rest of the path with '/' (not spaces)
          let path = "/" ++ intercalate "/" (drop 1 hostAndRest)
          return (isHttps, host, path, port, url)

fetchTodo :: MonadIO m => String -> m (Maybe Todo)
fetchTodo urlString = do
  case parseUrl urlString of
    Nothing -> do
      liftIO $ putStrLn $ "Invalid URL format: " ++ urlString
      return Nothing
    Just (isHttps, host, path, port, _) -> do
      liftIO $ putStrLn $ "Attempting to fetch from: " ++ urlString
      -- Perform the HTTP GET request and handle potential exceptions
      result <- liftIO $ try $ Req.runReq Req.defaultHttpConfig $ do
        let pathSegments = filter (not . null) $ splitString "/" (drop 1 path)
        if isHttps
          then do
            let baseUrl = Req.https (T.pack host)
            let url = foldl (Req./:) baseUrl (map T.pack pathSegments)
            response <- Req.req Req.GET url Req.NoReqBody Req.jsonResponse $
                        Req.port port
            let todo = Req.responseBody response :: Todo
            return todo
          else do
            let baseUrl = Req.http (T.pack host)
            let url = foldl (Req./:) baseUrl (map T.pack pathSegments)
            response <- Req.req Req.GET url Req.NoReqBody Req.jsonResponse $
                        Req.port port
            let todo = Req.responseBody response :: Todo
            return todo
      case result of
        Left (e :: SomeException) -> do
          liftIO $ putStrLn $ "HTTP request failed: " ++ show e
          return Nothing
        Right todo -> do
          liftIO $ putStrLn "Successfully fetched and parsed Todo."
          return $ Just todo

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted  = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted
