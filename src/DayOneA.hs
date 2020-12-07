-- DayOneA.hs

module DayOneA ( main, readReport ) where

import System.IO (readFile)
import Control.Monad (forM)


filename = "day1-input.txt"

main :: IO ()
main = do
  report <- readReport filename
  let pairs = [ (x, y) | x <- report, y <- report, x + y == 2020 ]
  printPair $ head pairs

printPair :: (Num a, Show a) => (a,a) -> IO ()
printPair (a, b) = print (a * b)

readReport :: String -> IO [Int]
readReport filename = map read . lines <$> readFile filename
