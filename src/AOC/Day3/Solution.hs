-- | Day 3 task
module Day3.Solution where

import FileInput (readStringList)

countTrees :: (Int, Int) -> Int -> [[Char]] -> Int
countTrees _ _ [] = 0
countTrees t@(x, y) l (row : rows) = target + countTrees t (l + 1) (drop (y - 1) rows)
  where
    target = fromEnum (cycle row !! (x * l) == '#')

slopeProduct :: [[Char]] -> Int
slopeProduct xs =
  product
    . map (\t -> countTrees t 0 xs)
    $ [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

d3p1 :: IO ()
d3p1 = do
  content <- readStringList "src/AOC/Day3/input.txt"
  print $ countTrees 3 1 0 content

d3p2 :: IO ()
d3p2 = do
  content <- readStringList "src/AOC/Day3/input.txt"
  print $ countSlopes content
