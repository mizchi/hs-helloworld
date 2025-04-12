import Lib
import Test.HUnit
import qualified System.Exit as Exit

test1 :: Test
test1 = TestCase (assertEqual "should return 3" 3 (add 1 2))

-- クイックソートのテストケース
quickSortTestEmpty :: Test
quickSortTestEmpty = TestCase (assertEqual "for empty list" ([] :: [Int]) (quicksort []))

quickSortTestSorted :: Test
quickSortTestSorted = TestCase (assertEqual "for sorted list" ([1, 2, 3] :: [Int]) (quicksort ([1, 2, 3] :: [Int])))

quickSortTestReverse :: Test
quickSortTestReverse = TestCase (assertEqual "for reverse sorted list" ([1, 2, 3] :: [Int]) (quicksort ([3, 2, 1] :: [Int])))

quickSortTestRandom :: Test
quickSortTestRandom = TestCase (assertEqual "for random list" ([1, 1, 2, 3, 4, 5, 6, 9] :: [Int]) (quicksort ([3, 1, 4, 1, 5, 9, 2, 6] :: [Int])))

-- 全テストをまとめる
tests :: Test
tests = TestList [
    TestLabel "add test" test1,
    TestLabel "quicksort empty" quickSortTestEmpty,
    TestLabel "quicksort sorted" quickSortTestSorted,
    TestLabel "quicksort reverse" quickSortTestReverse,
    TestLabel "quicksort random" quickSortTestRandom
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess