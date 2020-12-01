-- | Module for reading in task inputs
-- and parsing them into the correct
-- datatypes
module FileInput (readIntList) where

readIntList :: FilePath -> IO [Int]
readIntList filepath = do
  content <- readFile filepath
  return . map read . lines $ content
