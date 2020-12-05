-- | Day 5 task
module Day5.Solution where

import Data.List ((\\), sort)
import FileInput (readStringList)

data Row = F | B deriving (Show)

data Column = R | L deriving (Show)

findRow :: (Int, Int) -> [Row] -> Int
findRow _ [] = 0
findRow (r1, r2) [d] =
  case d of
    F -> r1
    B -> r2
findRow (from, to) (F : ds) =
  findRow (from, to - (to + 1 - from) `div` 2) ds
findRow (from, to) (B : ds) =
  findRow (from + (to + 1 - from) `div` 2, to) ds

findColumn :: (Int, Int) -> [Column] -> Int
findColumn _ [] = 0
findColumn (r1, r2) [d] =
  case d of
    L -> r1
    R -> r2
findColumn (from, to) (L : ds) =
  findColumn (from, to - (to + 1 - from) `div` 2) ds
findColumn (from, to) (R : ds) =
  findColumn (from + (to + 1 - from) `div` 2, to) ds

filterDirs :: String -> ([Row], [Column])
filterDirs = go ([], [])
  where
    go (rs, cs) [] = (reverse rs, reverse cs)
    go (rs, cs) ('F' : ds) = go (F : rs, cs) ds
    go (rs, cs) ('B' : ds) = go (B : rs, cs) ds
    go (rs, cs) ('R' : ds) = go (rs, R : cs) ds
    go (rs, cs) ('L' : ds) = go (rs, L : cs) ds
    go _ _ = error "impossible"

solver :: String -> Int
solver xs = findRow (0, 127) rs * 8 + findColumn (0, 7) cs
  where
    (rs, cs) = filterDirs xs

solve :: [String] -> Int
solve = maximum . map solver

findSeat :: [String] -> Int
findSeat ss = head $ calc \\ list
  where
    calc = [head list .. last list]
    list = sort $ map solver ss

d5p1 :: IO ()
d5p1 = do
  lineList <- readStringList "src/AOC/Day5/input.txt"
  putStrLn $ "Day 5 Part 1: " ++ show (solve lineList)

d5p2 :: IO ()
d5p2 = do
  lineList <- readStringList "src/AOC/Day5/input.txt"
  putStrLn $ "Day 5 Part 2: " ++ show (findSeat lineList)
