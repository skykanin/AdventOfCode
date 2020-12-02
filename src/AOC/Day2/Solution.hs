-- |
module AOC.Day2.Solution where

import FileInput (readStringList)

type Policy = (Int, Int, Char, String)

parsePolicy :: String -> Policy
parsePolicy s = (read m, read n, char, passwd)
  where
    (m, t) = span (/= '-') s
    (n, t') = span (/= ' ') (tail t)
    (char : ':' : ' ' : passwd) = tail t'

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
    xor p q = (p && not q) || (not p && q)

d2p1 :: IO ()
d2p1 = do
  content <- readStringList "src/AOC/Day2/input.txt"
  print (countValid validPassword content)

d2p2 :: IO ()
d2p2 = do
  content <- readStringList "src/AOC/Day2/input.txt"
  print (countValid validPassword2 content)
