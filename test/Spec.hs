{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Import FetchError type

-- Need runReq for tests
import Control.Monad (unless) -- For conditional assertion failure
import Lib (FetchError (..), Todo (..), fetchTodo)
import qualified Network.HTTP.Req as Req
import qualified System.Exit as Exit
import Test.HUnit

-- Helper function to run fetchTodo within the Req context for tests
runFetchTodo :: String -> IO (Either FetchError Todo)
runFetchTodo url = Req.runReq Req.defaultHttpConfig $ fetchTodo url

-- Expected Todo data for the success case
expectedTodo1 :: Todo
expectedTodo1 =
  Todo
    { userId = 1,
      Lib.id = 1, -- Qualify 'id' to avoid clash with prelude function
      title = "delectus aut autem",
      completed = False
    }

-- Test case for a successful fetch
testFetchSuccess :: Test
testFetchSuccess = TestCase $ do
  putStrLn "\nRunning testFetchSuccess (requires internet)..."
  let url = "https://jsonplaceholder.typicode.com/todos/1"
  result <- runFetchTodo url
  -- Use assertBool for a more informative failure message
  assertBool
    ("Expected Right " ++ show expectedTodo1 ++ ", but got " ++ show result ++ " for URL: " ++ url)
    (result == Right expectedTodo1)

-- Test case for an invalid URL format
testInvalidUrlFormat :: Test
testInvalidUrlFormat = TestCase $ do
  putStrLn "\nRunning testInvalidUrlFormat..."
  let url = "this is not a url"
  result <- runFetchTodo url
  case result of
    Left (InvalidUrlFormat _) ->
      assertBool "Should be InvalidUrlFormat" True
    _ ->
      assertFailure $ "Expected InvalidUrlFormat, but got: " ++ show result

-- Test case for a non-existent host
testNonExistentHost :: Test
testNonExistentHost = TestCase $ do
  putStrLn "\nRunning testNonExistentHost (requires internet)..."
  -- Use a TLD that is reserved for testing/documentation and should not resolve
  let url = "https://example.invalid/todos/1"
  result <- runFetchTodo url
  case result of
    Left (NetworkError _) ->
      assertBool "Should be NetworkError" True
    _ ->
      assertFailure $ "Expected NetworkError, but got: " ++ show result

-- Test case for a 404 Not Found error
testNotFound :: Test
testNotFound = TestCase $ do
  putStrLn "\nRunning testNotFound (requires internet)..."
  let url = "https://jsonplaceholder.typicode.com/todos/99999999" -- Assume this ID doesn't exist
  result <- runFetchTodo url
  case result of
    Left (HttpError _) ->
      assertBool "Should be HttpError" True
    _ ->
      assertFailure $ "Expected HttpError, but got: " ++ show result

-- List of all tests
tests :: Test
tests =
  TestList
    [ TestLabel "fetch success" testFetchSuccess,
      TestLabel "invalid URL format" testInvalidUrlFormat,
      TestLabel "non-existent host" testNonExistentHost,
      TestLabel "404 Not Found" testNotFound
    ]

main :: IO ()
main = do
  putStrLn "Running HTTP Todo Fetcher Tests..."
  testResults <- runTestTT tests -- Renamed to avoid shadowing Test.HUnit.counts
  -- Exit with failure if any tests failed or had errors
  unless
    (errors testResults == 0 && failures testResults == 0)
    Exit.exitFailure
