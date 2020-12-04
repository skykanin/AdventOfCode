-- | Day 4 task
module Day4.Solution where

import Data.Either (lefts, rights)
import Data.List (groupBy)
import qualified Data.Set as S
import Data.Text (Text, pack)
import FileInput (readString)
import Text.Parsec (try)
import Text.Parsec.Text (Parser)
import Text.ParserCombinators.Parsec hiding (Parser, try)

groupPassports :: String -> [[String]]
groupPassports = map (concatMap words) . filter pluck . groupBy empty . lines
  where
    empty _ "" = False
    empty "" _ = False
    empty _ _ = True
    pluck [""] = False
    pluck _ = True

countValidPassports :: [[String]] -> Int
countValidPassports = length . filter id . map (valid . map (take 3))
  where
    valid p = null missing
      where
        missing = fields `S.difference` S.fromList p
        fields = S.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

solutionP1 :: String -> Int
solutionP1 = countValidPassports . groupPassports

data Unit = CM | IN
  deriving (Eq, Show)

data Height = Height Int Unit
  deriving (Eq, Show)

data Passport = Passport
  { _byr :: Int,
    _iyr :: Int,
    _eyr :: Int,
    _hgt :: Height,
    _hcl :: String,
    _ecl :: String,
    _pid :: String
  }
  deriving (Eq, Show)

byr :: Parser Int
byr = read <$> count 4 digit

iyr :: Parser Int
iyr = byr

eyr :: Parser Int
eyr = byr

number :: Parser Int
number = read <$> many1 digit

suffix :: Parser Unit
suffix = toUnit <$> (cm <|> inch) <* eof
  where
    cm = string "cm"
    inch = string "in"
    toUnit "cm" = CM
    toUnit "in" = IN
    toUnit _ = error "Impossible"

hgt :: Parser Height
hgt = Height <$> number <*> suffix

hcl :: Parser String
hcl = (:) <$> char '#' <*> hunk
  where
    hunk = count 6 (digit <|> oneOf ['a' .. 'f']) <* eof

ecl :: Parser String
ecl = choice $ map (try . threeStr) colours
  where
    threeStr :: String -> Parser String
    threeStr s = string s <* eof
    colours = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

pid :: Parser String
pid = count 9 digit <* eof

headRight :: [Either a b] -> Either a b
headRight eithers
  | null (rights eithers) = Left . head $ lefts eithers
  | otherwise = Right . head $ rights eithers

apply :: String -> Parser a -> SourceName -> [Text] -> Either ParseError a
apply key p e = headRight . map (parse (try fieldParser) e)
  where
    fieldParser = string (key ++ ":") *> p

parsePassport :: [Text] -> Either ParseError Passport
parsePassport strs = do
  b <- go "byr" byr
  i <- go "iyr" iyr
  e <- go "eyr" eyr
  hg <- go "hgt" hgt
  hc <- go "hcl" hcl
  ec <- go "ecl" ecl
  p <- go "pid" pid
  return $ Passport b i e hg hc ec p
  where
    go pre p = apply pre p "Error" strs

validPassport :: Passport -> Bool
validPassport (Passport b i e (Height hg u) _ _ _) =
  b >= 1920 && b <= 2002
    && i >= 2010
    && i <= 2020
    && e >= 2020
    && e <= 2030
    && height hg u
  where
    height v CM = v >= 150 && v <= 193
    height v IN = v >= 59 && v <= 76

solutionP2 :: String -> Int
solutionP2 =
  length
    . filter validPassport
    . rights
    . map (parsePassport . (map pack))
    . groupPassports

d4p1 :: IO ()
d4p1 = do
  str <- readString "src/AOC/Day4/input.txt"
  print $ solutionP1 str

d4p2 :: IO ()
d4p2 = do
  str <- readString "src/AOC/Day4/input.txt"
  print $ solutionP2 str
