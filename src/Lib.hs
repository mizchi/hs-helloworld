{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( Todo (..),
    fetchTodo,
    quicksort,
    FetchError (..), -- Export the FetchError type
  )
where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Network.HTTP.Req as Req
import Text.Read (readMaybe)

-- | Represents a Todo item fetched from the API.
data Todo = Todo
  { userId :: Int,
    id :: Int,
    title :: Text,
    completed :: Bool
  }
  deriving (Generic, Show, Eq)

-- | Automatically derive FromJSON instance for parsing JSON into Todo.
instance Aeson.FromJSON Todo

-- | Represents errors that can occur during fetching.
data FetchError
  = -- | URL format is invalid
    InvalidUrlFormat String
  | -- | Network error occurred
    NetworkError String
  | -- | HTTP error occurred
    HttpError String
  | -- | JSON parsing error
    ParseError String
  deriving (Show, Eq)

-- | Split a string by a delimiter
splitString :: String -> String -> [String]
splitString _ [] = []
splitString delim str =
  case findDelim delim str of
    Nothing -> [str] -- No delimiter found, return the whole string
    Just i ->
      let (before, after) = splitAt i str
          afterDelim = drop (length delim) after
       in before : splitString delim afterDelim

-- | Find the position of a delimiter in a string
findDelim :: String -> String -> Maybe Int
findDelim _ [] = Nothing
findDelim delim str@(_ : xs)
  | take (length delim) str == delim = Just 0
  | otherwise = case findDelim delim xs of
      Nothing -> Nothing
      Just i -> Just (i + 1)

-- | Parse a URL string into components (scheme, host, path, etc.)
-- Returns Nothing if the URL is invalid
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
          let port =
                if length hostAndPort > 1
                  then fromMaybe (if isHttps then 443 else 80) (readMaybe (hostAndPort !! 1))
                  else (if isHttps then 443 else 80)
          -- Join the rest of the path with '/' (not spaces)
          let path = "/" ++ intercalate "/" (drop 1 hostAndRest)

          return (isHttps, host, path, port, url)

-- | Fetches a Todo item from the specified URL string.
-- Returns Either FetchError Todo: Right Todo on success, Left FetchError on failure.
fetchTodo :: (MonadIO m) => String -> m (Either FetchError Todo)
fetchTodo urlString = do
  case parseUrl urlString of
    Nothing -> do
      return $ Left $ InvalidUrlFormat urlString
    Just (isHttps, host, path, port, _) -> do
      -- Perform the HTTP GET request and handle potential exceptions
      result <- liftIO $ try $ Req.runReq Req.defaultHttpConfig $ do
        -- Split the path into segments and remove empty segments
        let pathSegments = filter (not . null) $ splitString "/" (drop 1 path)

        if isHttps
          then do
            -- HTTPS request
            -- Build the URL by folding over path segments
            let baseUrl = Req.https (T.pack host)
            let url = foldl (Req./:) baseUrl (map T.pack pathSegments)
            response <-
              Req.req Req.GET url Req.NoReqBody Req.jsonResponse $
                Req.port port
            let todo = Req.responseBody response :: Todo
            return todo
          else do
            -- HTTP request
            -- Build the URL by folding over path segments
            let baseUrl = Req.http (T.pack host)
            let url = foldl (Req./:) baseUrl (map T.pack pathSegments)
            response <-
              Req.req Req.GET url Req.NoReqBody Req.jsonResponse $
                Req.port port
            let todo = Req.responseBody response :: Todo
            return todo

      -- Process the result
      case result of
        Left (e :: SomeException) -> do
          -- Determine the type of error based on the exception message
          let errorMsg = show e
          let fetchError =
                if "ConnectionFailure" `isInfixOf` errorMsg
                  then NetworkError errorMsg
                  else
                    if "StatusCodeException" `isInfixOf` errorMsg
                      then HttpError errorMsg
                      else
                        if "JSONError" `isInfixOf` errorMsg
                          then ParseError errorMsg
                          else NetworkError errorMsg
          return $ Left fetchError
        Right todo -> do
          return $ Right todo

-- | Check if a substring is contained within a string
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    tails [] = [[]]
    tails xs@(_ : xs') = xs : tails xs'

    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

-- | クイックソート関数
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
   in smallerSorted ++ [x] ++ biggerSorted
