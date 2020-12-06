-- |
module Day6.Solution where

import qualified Data.HashMap.Strict as M
import Data.List (groupBy)
import qualified Data.Set as S
import FileInput (readString)

groupGroups :: String -> [[String]]
groupGroups = filter fullStr . groupBy empty . lines
  where
    empty [] _ = False
    empty _ [] = False
    empty _ _ = True
    fullStr [""] = False
    fullStr _ = True

countYes :: String -> Int
countYes = go S.empty
  where
    go s "" = S.size s
    go s (x : xs) = go (S.insert x s) xs

sumYes :: String -> Int
sumYes = sum . map (countYes . concat) . groupGroups

countAllYes :: [String] -> Int
countAllYes xs = length . filter (== len) . M.elems . foldr insert M.empty $ xs
  where
    len = length xs
    insert str m = go str m
    go [] m = m
    go (c : cs) m = go cs $ M.insertWith (+) c 1 m

sumAllYes :: String -> Int
sumAllYes = sum . map countAllYes . groupGroups

d6p1 :: IO ()
d6p1 = do
  str <- readString "src/AOC/Day6/input.txt"
  putStrLn $ "Day 6 Part 1: " ++ show (sumYes str)

d6p2 :: IO ()
d6p2 = do
  str <- readString "src/AOC/Day6/input.txt"
  putStrLn $ "Day 6 Part 2: " ++ show (sumAllYes str)
