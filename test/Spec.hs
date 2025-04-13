import Control.Monad (unless) -- For conditional assertion failure
import Lib
  ( FetchError (..),
    Todo (..),
    concurrentFetch2,
    fetchTodo,
    fetchWithTimeout,
    waitFetchResult,
  )
import SortSpec (sortTests)
import qualified System.Exit as Exit
import Test.HUnit

-- Helper function to run fetchTodo within the Req context for tests
runFetchTodo :: String -> IO (Either FetchError Todo)
runFetchTodo url = do
  asyncResult <- fetchTodo url
  waitFetchResult asyncResult

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

-- Test case for concurrent fetching
testConcurrentFetch :: Test
testConcurrentFetch = TestCase $ do
  putStrLn "\nRunning testConcurrentFetch (requires internet)..."
  let url1 = "https://jsonplaceholder.typicode.com/todos/1"
      url2 = "https://jsonplaceholder.typicode.com/todos/2"
  (result1, result2) <- Lib.concurrentFetch2 url1 url2

  -- Check first result
  assertBool
    ("Expected Right Todo for URL1, but got " ++ show result1)
    ( case result1 of
        Right todo -> userId todo == 1 && Lib.id todo == 1
        _ -> False
    )

  -- Check second result
  assertBool
    ("Expected Right Todo for URL2, but got " ++ show result2)
    ( case result2 of
        Right todo -> userId todo == 1 && Lib.id todo == 2
        _ -> False
    )

-- Test case for timeout (success case)
testTimeoutSuccess :: Test
testTimeoutSuccess = TestCase $ do
  putStrLn "\nRunning testTimeoutSuccess (requires internet)..."
  let url = "https://jsonplaceholder.typicode.com/todos/1"
      timeoutMicros = 5000000 -- 5 seconds should be enough
  result <- Lib.fetchWithTimeout timeoutMicros url

  assertBool
    ("Expected Right Todo, but got " ++ show result)
    ( case result of
        Right todo -> userId todo == 1 && Lib.id todo == 1
        _ -> False
    )

-- Test case for timeout (failure case)
testTimeoutFailure :: Test
testTimeoutFailure = TestCase $ do
  putStrLn "\nRunning testTimeoutFailure (requires internet)..."
  let url = "https://jsonplaceholder.typicode.com/todos/1"
      timeoutMicros = 1 -- 1 microsecond is definitely not enough
  result <- Lib.fetchWithTimeout timeoutMicros url

  case result of
    Left (NetworkError msg) -> do
      assertBool
        ("Expected timeout message, but got: " ++ msg)
        ("Timeout" `isInfixOf` msg)
    _ ->
      assertFailure $ "Expected NetworkError with timeout message, but got: " ++ show result
  where
    -- Helper function to check if a substring is in a string
    isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

    tails [] = [[]]
    tails xs@(_ : xs') = xs : tails xs'

    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x : xs) (y : ys) = x == y && isPrefixOf xs ys

-- List of all tests
tests :: Test
tests =
  TestList
    [ TestLabel "fetch success" testFetchSuccess,
      TestLabel "invalid URL format" testInvalidUrlFormat,
      TestLabel "non-existent host" testNonExistentHost,
      TestLabel "404 Not Found" testNotFound,
      TestLabel "concurrent fetch" testConcurrentFetch,
      TestLabel "timeout success" testTimeoutSuccess,
      TestLabel "timeout failure" testTimeoutFailure,
      TestLabel "sort tests" sortTests
    ]

main :: IO ()
main = do
  putStrLn "Running HTTP Todo Fetcher Tests..."
  testResults <- runTestTT tests -- Renamed to avoid shadowing Test.HUnit.counts
  -- Exit with failure if any tests failed or had errors
  unless
    (errors testResults == 0 && failures testResults == 0)
    Exit.exitFailure
