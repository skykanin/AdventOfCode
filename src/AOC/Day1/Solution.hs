-- | Day1 task
module Day1.Solution
  ( d1p1,
    d1p2,
  )
where

import qualified Data.IntSet as S
import FileInput (readIntList)

-- | Linear runtime implementation using an IntSet
findSumOf2 :: [Int] -> Int
findSumOf2 = fst . foldr f (0, S.empty)
  where
    f n (prod, set)
      | S.member (2020 - n) set = ((2020 - n) * n, set)
      | otherwise = (prod, S.insert n set)

-- | Quadratic runtime implementation using the same method as above
findSumOf3 :: [Int] -> Int
findSumOf3 expenses = fst $ foldr f (0, S.empty) expenses
  where
    f n (prod, set) =
      case findSum expenses of
        [] -> (prod, S.insert n set)
        (m : _) -> ((2020 - n - m) * n * m, set)
      where
        findSum = filter (\k -> S.member (2020 - n - k) set)

d1p1 :: IO ()
d1p1 = do
  expenses <- readIntList "src/AOC/Day1/input.txt"
  print $ findSumOf2 expenses

d1p2 :: IO ()
d1p2 = do
  expenses <- readIntList "src/AOC/Day1/input.txt"
  print $ findSumOf3 expenses
