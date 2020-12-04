-- |
module Day2.Solution
  ( d2p1,
    d2p2,
  )
where

import Data.Bits (xor)
import FileInput (readStringList)

type Policy = (Int, Int, Char, String)

-- | Parse happy path
parsePolicy :: String -> Policy
parsePolicy s = (read m, read n, char, passwd)
  where
    (m, n) = tail <$> span (/= '-') nums
    [nums, char : _, passwd] = words s

validPassword :: Policy -> Bool
validPassword (m, n, char, passwd) =
  count char passwd >= m && count char passwd <= n
  where
    count el = length . filter (== el)

countValid :: (Policy -> Bool) -> [String] -> Int
countValid policy = length . filter (policy . parsePolicy)

validPassword2 :: Policy -> Bool
validPassword2 (m, n, char, passwd) = check m `xor` check n
  where
    check i = passwd !! (i - 1) == char

d2p1 :: IO ()
d2p1 = do
  content <- readStringList "src/AOC/Day2/input.txt"
  putStrLn ("Day 2 Part 1: " ++ show (countValid validPassword content))

d2p2 :: IO ()
d2p2 = do
  content <- readStringList "src/AOC/Day2/input.txt"
  putStrLn ("Day 2 Part 2: " ++ show (countValid validPassword2 content))
