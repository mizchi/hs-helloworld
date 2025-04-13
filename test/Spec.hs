{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Lib (fetchTodo, Todo(..)) -- Import necessary items from Lib
import Test.HUnit
import qualified System.Exit as Exit
import qualified Network.HTTP.Req as Req -- Need runReq for tests
-- Data.Text is not directly used, removed redundant import
import Control.Monad (unless) -- For conditional assertion failure

-- Helper function to run fetchTodo within the Req context for tests
runFetchTodo :: String -> IO (Maybe Todo)
runFetchTodo url = Req.runReq Req.defaultHttpConfig $ fetchTodo url

-- Expected Todo data for the success case
expectedTodo1 :: Todo
expectedTodo1 = Todo
  { userId = 1
  , Lib.id = 1 -- Qualify 'id' to avoid clash with prelude function
  , title = "delectus aut autem"
  , completed = False
  }

-- Test case for a successful fetch
testFetchSuccess :: Test
testFetchSuccess = TestCase $ do
    putStrLn "\nRunning testFetchSuccess (requires internet)..."
    let url = "https://jsonplaceholder.typicode.com/todos/1"
    maybeTodo <- runFetchTodo url
    -- Use assertBool for a more informative failure message with Maybe
    assertBool ("Expected Just " ++ show expectedTodo1 ++ ", but got " ++ show maybeTodo ++ " for URL: " ++ url) (maybeTodo == Just expectedTodo1)

-- Test case for an invalid URL format
testInvalidUrlFormat :: Test
testInvalidUrlFormat = TestCase $ do
    putStrLn "\nRunning testInvalidUrlFormat..."
    let url = "this is not a url"
    maybeTodo <- runFetchTodo url
    assertEqual "for invalid URL format" Nothing maybeTodo

-- Test case for a non-existent host
testNonExistentHost :: Test
testNonExistentHost = TestCase $ do
    putStrLn "\nRunning testNonExistentHost (requires internet)..."
    -- Use a TLD that is reserved for testing/documentation and should not resolve
    let url = "https://example.invalid/todos/1"
    maybeTodo <- runFetchTodo url
    assertEqual "for non-existent host" Nothing maybeTodo

-- Test case for a 404 Not Found error
testNotFound :: Test
testNotFound = TestCase $ do
    putStrLn "\nRunning testNotFound (requires internet)..."
    let url = "https://jsonplaceholder.typicode.com/todos/99999999" -- Assume this ID doesn't exist
    maybeTodo <- runFetchTodo url
    -- Expect Nothing because the JSON parsing within req/Lib should fail or be caught
    assertEqual "for 404 Not Found" Nothing maybeTodo


-- List of all tests
tests :: Test
tests = TestList [
    TestLabel "fetch success" testFetchSuccess,
    TestLabel "invalid URL format" testInvalidUrlFormat,
    TestLabel "non-existent host" testNonExistentHost,
    TestLabel "404 Not Found" testNotFound
    ]

main :: IO ()
main = do
    putStrLn "Running HTTP Todo Fetcher Tests..."
    testResults <- runTestTT tests -- Renamed to avoid shadowing Test.HUnit.counts
    -- Exit with failure if any tests failed or had errors
    unless (errors testResults == 0 && failures testResults == 0) $
      Exit.exitFailure