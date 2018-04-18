module IntCSV
    ( readCSV, writeCSV, stdreadCSV, stdwriteCSV, parseAsCSV, CSV(..)
    ) where


import System.IO (isEOF)
import Data.String.Utils
import Data.List (intercalate)
import WhileM

newtype CSV = CSV [[Int]]

readCSV :: FilePath -> IO CSV
readCSV p = fmap parseAsCSV (readFile p)

writeCSV :: FilePath -> CSV -> IO ()
writeCSV p csv = writeFile p (encodeCSV csv)

stdreadCSV :: IO CSV
stdreadCSV = fmap parseLinesAsCSV stdinToLines where
  stdinToLines = fmap reverse stdinToLinesR
  stdinToLinesR :: IO [String]
  stdinToLinesR = whileMFoldL [] (\(b,a) -> a : b) getLine (fmap not isEOF)

stdwriteCSV :: CSV -> IO ()
stdwriteCSV csv = putStr $ encodeCSV csv

parseAsCSV :: String -> CSV
parseAsCSV s = parseLinesAsCSV $ lines s

parseLinesAsCSV :: [String] -> CSV
parseLinesAsCSV l = CSV $ fmap splitColumns l where
  splitColumns :: String -> [Int]
  splitColumns s = fmap toInt (split "," s)
  toInt :: String -> Int
  toInt = read . strip

encodeCSV :: CSV -> String
encodeCSV (CSV rows) = intercalate "\n" lines where
  lines = fmap toLine rows
  toLine :: [Int] -> String
  toLine ints = intercalate ", " (fmap show ints)