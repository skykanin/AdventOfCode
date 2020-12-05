-- | Day 5 task
module Day5.Solution where

import Data.List ((\\))
import FileInput (readStringList)

findSeatId :: String -> Int
findSeatId = foldl f 0
  where
    f n c = case c of
      'B' -> 2 * n + 1
      'R' -> 2 * n + 1
      'L' -> 2 * n
      'F' -> 2 * n
      _ -> n

solveP1 :: [String] -> Int
solveP1 = maximum . map findSeatId

findSeat :: [String] -> Int
findSeat ss = head $ calc \\ ids
  where
    calc = [minimum ids .. maximum ids]
    ids = map findSeatId ss

d5p1 :: IO ()
d5p1 = do
  lineList <- readStringList "src/AOC/Day5/input.txt"
  putStrLn $ "Day 5 Part 1: " ++ show (solveP1 lineList)

d5p2 :: IO ()
d5p2 = do
  lineList <- readStringList "src/AOC/Day5/input.txt"
  putStrLn $ "Day 5 Part 2: " ++ show (findSeat lineList)
