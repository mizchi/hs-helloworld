module SortSpec (sortTests) where

import Data.List (sort)
import Sort (mergesort, quicksort)
import Test.HUnit
import Test.QuickCheck

-- | ソート関数のテスト用データ
testData :: [Int]
testData = [5, 1, 9, 4, 6, 7, 3, 2, 8]

-- | ソート済みのデータ
sortedData :: [Int]
sortedData = [1, 2, 3, 4, 5, 6, 7, 8, 9]

-- | 空のリストのテスト
testEmptyList :: Test
testEmptyList = TestCase $ do
  putStrLn "\nRunning testEmptyList..."
  let result1 = quicksort ([] :: [Int])
      result2 = mergesort ([] :: [Int])
  assertEqual "quicksort empty list" [] result1
  assertEqual "mergesort empty list" [] result2

-- | 単一要素のリストのテスト
testSingleElementList :: Test
testSingleElementList = TestCase $ do
  putStrLn "\nRunning testSingleElementList..."
  let result1 = quicksort [42]
      result2 = mergesort [42]
  assertEqual "quicksort single element" [42] result1
  assertEqual "mergesort single element" [42] result2

-- | 整列済みリストのテスト
testSortedList :: Test
testSortedList = TestCase $ do
  putStrLn "\nRunning testSortedList..."
  let result1 = quicksort sortedData
      result2 = mergesort sortedData
  assertEqual "quicksort sorted list" sortedData result1
  assertEqual "mergesort sorted list" sortedData result2

-- | 逆順リストのテスト
testReversedList :: Test
testReversedList = TestCase $ do
  putStrLn "\nRunning testReversedList..."
  let reversedData = reverse sortedData
      result1 = quicksort reversedData
      result2 = mergesort reversedData
  assertEqual "quicksort reversed list" sortedData result1
  assertEqual "mergesort reversed list" sortedData result2

-- | ランダムリストのテスト
testRandomList :: Test
testRandomList = TestCase $ do
  putStrLn "\nRunning testRandomList..."
  let result1 = quicksort testData
      result2 = mergesort testData
  assertEqual "quicksort random list" sortedData result1
  assertEqual "mergesort random list" sortedData result2

-- | 重複要素を含むリストのテスト
testDuplicatesList :: Test
testDuplicatesList = TestCase $ do
  putStrLn "\nRunning testDuplicatesList..."
  let duplicatesData = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
      sortedDuplicatesData = [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9]
      result1 = quicksort duplicatesData
      result2 = mergesort duplicatesData
  assertEqual "quicksort duplicates list" sortedDuplicatesData result1
  assertEqual "mergesort duplicates list" sortedDuplicatesData result2

-- | 負の数を含むリストのテスト
testNegativesList :: Test
testNegativesList = TestCase $ do
  putStrLn "\nRunning testNegativesList..."
  let negativesData = [3, -1, 4, -1, -5, 9, -2, 6, 5, -3, 5]
      sortedNegativesData = [-5, -3, -2, -1, -1, 3, 4, 5, 5, 6, 9]
      result1 = quicksort negativesData
      result2 = mergesort negativesData
  assertEqual "quicksort negatives list" sortedNegativesData result1
  assertEqual "mergesort negatives list" sortedNegativesData result2

-- | 文字列のリストのテスト
testStringsList :: Test
testStringsList = TestCase $ do
  putStrLn "\nRunning testStringsList..."
  let stringsData = ["banana", "apple", "cherry", "date", "elderberry"]
      sortedStringsData = ["apple", "banana", "cherry", "date", "elderberry"]
      result1 = quicksort stringsData
      result2 = mergesort stringsData
  assertEqual "quicksort strings list" sortedStringsData result1
  assertEqual "mergesort strings list" sortedStringsData result2

-- | QuickCheck を使ったプロパティテスト

-- | ソート後のリストは昇順になっているかをテストするプロパティ
prop_isSorted :: ([Int] -> [Int]) -> [Int] -> Bool
prop_isSorted sortFunc xs = isSorted (sortFunc xs)
  where
    isSorted [] = True
    isSorted [_] = True
    isSorted (x : y : xs) = x <= y && isSorted (y : xs)

-- | ソート後のリストの長さは元のリストと同じかをテストするプロパティ
prop_sameLength :: ([Int] -> [Int]) -> [Int] -> Bool
prop_sameLength sortFunc xs = length (sortFunc xs) == length xs

-- | ソート後のリストには元のリストと同じ要素が含まれているかをテストするプロパティ
-- | （順序を無視して要素の出現回数が同じかをテスト）
prop_sameElements :: ([Int] -> [Int]) -> [Int] -> Bool
prop_sameElements sortFunc xs = sort (sortFunc xs) == sort xs

-- | ソート操作を2回適用しても結果は変わらないかをテストするプロパティ
prop_idempotent :: ([Int] -> [Int]) -> [Int] -> Bool
prop_idempotent sortFunc xs = sortFunc (sortFunc xs) == sortFunc xs

-- | QuickCheck テストを実行する関数
runQuickCheckTests :: Test
runQuickCheckTests = TestCase $ do
  putStrLn "\nRunning QuickCheck tests..."

  -- quicksort のプロパティテスト
  putStrLn "Testing quicksort properties:"
  quickCheck (prop_isSorted quicksort)
  quickCheck (prop_sameLength quicksort)
  quickCheck (prop_sameElements quicksort)
  quickCheck (prop_idempotent quicksort)

  -- mergesort のプロパティテスト
  putStrLn "Testing mergesort properties:"
  quickCheck (prop_isSorted mergesort)
  quickCheck (prop_sameLength mergesort)
  quickCheck (prop_sameElements mergesort)
  quickCheck (prop_idempotent mergesort)

-- | ソートのテストリスト
sortTests :: Test
sortTests =
  TestList
    [ TestLabel "empty list" testEmptyList,
      TestLabel "single element list" testSingleElementList,
      TestLabel "sorted list" testSortedList,
      TestLabel "reversed list" testReversedList,
      TestLabel "random list" testRandomList,
      TestLabel "duplicates list" testDuplicatesList,
      TestLabel "negatives list" testNegativesList,
      TestLabel "strings list" testStringsList,
      TestLabel "quickcheck tests" runQuickCheckTests
    ]
