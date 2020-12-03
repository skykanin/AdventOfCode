-- | Day 3 task
module AOC.Day3.Solution where

import FileInput (readStringList)

countTrees :: Int -> Int -> Int -> [[Char]] -> Int
countTrees _ _ _ [] = 0
countTrees r d n (x : xs) = target + countTrees r d (n + 1) (drop (d - 1) xs)
  where
    target = fromEnum (cycle x !! (r * n) == '#')

countSlopes :: [[Char]] -> Int
countSlopes xs =
  product
    . map (\(r, d) -> countTrees r d 0 xs)
    $ [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

d3p1 :: IO ()
d3p1 = do
  content <- readStringList "src/AOC/Day3/input.txt"
  print $ countTrees 3 1 0 content

d3p2 :: IO ()
d3p2 = do
  content <- readStringList "src/AOC/Day3/input.txt"
  print $ countSlopes content
