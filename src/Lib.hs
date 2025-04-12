module Lib (add, quicksort) where
add :: (Num a) => a -> a -> a
add x y = x + y

-- クイックソート関数
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = [] -- 空リストの場合は空リストを返す
quicksort (x:xs) = -- 先頭要素 x と残りのリスト xs に分割
    let smallerSorted = quicksort [a | a <- xs, a <= x] -- x以下の要素を再帰的にソート
        biggerSorted  = quicksort [a | a <- xs, a > x]  -- xより大きい要素を再帰的にソート
    in  smallerSorted ++ [x] ++ biggerSorted -- ソート済みリストを結合