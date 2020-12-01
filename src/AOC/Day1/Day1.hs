-- | Day1 task
module Day1.Day1
  ( d1p1,
    d1p2
  )
where

import FileInput (readIntList)
import qualified Data.IntSet as S

-- | Linear runtime implementation using an IntSet
findSumOf2Faster :: [Int] -> Int
findSumOf2Faster = fst . foldr f (0, S.empty)
  where
    f n (prod, set)
      | S.member (2020 - n) set = ((2020 - n) * n, set)
      | otherwise = (prod, S.insert n set)

findSumOf3 :: [Int] -> Int
findSumOf3 expenses =
  head
    [ n * m * k
      | n <- expenses,
        m <- expenses,
        k <- expenses,
        n + m + k == 2020
    ]

d1p1 :: IO ()
d1p1 = do
  expenses <- readIntList "src/AOC/Day1/input.txt"
  print $ findSumOf2Faster expenses

d1p2 :: IO ()
d1p2 = do
  expenses <- readIntList "src/AOC/Day1/input.txt"
  print $ findSumOf3 expenses
