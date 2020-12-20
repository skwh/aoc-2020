-- DayFiveA.hs
module DayFiveA (main) where

import Data.List (sort)

filename = "input/day5-input.txt"

readPuzzleInput :: String -> IO [String]
readPuzzleInput path = lines <$> readFile path

newtype Seat = Seat { getSeat :: (Int, Int, Int) } deriving (Eq, Show)

third :: (a, b, c) -> c
third (_, _, c) = c

seatId :: Seat -> Int
seatId = third.getSeat

instance Ord Seat where
  (<=) seatA seatB = asid <= bsid
    where (_, _, asid) = getSeat seatA
          (_, _, bsid) = getSeat seatB

type Range = (Int, Int)

readPass :: String -> Seat
readPass = readPassHelper ((0, 127), (0, 7))

readPassHelper :: (Range, Range) -> String -> Seat
readPassHelper (rrange, crange) str = Seat (row, col, seatId)
  where (rs, cs) = span isRowChar str
        row = readL 'F' rrange rs
        col = readL 'L' crange cs
        seatId = (row * 8) + col

isRowChar c = c == 'F' || c == 'B'

readL :: Char -> Range -> String -> Int
readL _ (_, max) [] = max
readL t (min, max) (c:cs)
  | c == t = readL t (min, hlf) cs
  | otherwise = readL t (hlf+1, max) cs
  where hlf = min + ((max - min) `div` 2)

offByTwo :: Int -> Int -> Bool
offByTwo a b = abs (a - b) == 2

findMissing :: [Seat] -> Int
findMissing ss = fm (sort ss)
  where fm [] = -1
        fm [_] = -1
        fm (a:b:cs) = let aid = seatId a
                          bid = seatId b
                      in if offByTwo aid bid
                         then aid+1
                         else fm (b:cs)

main :: IO ()
main = do
  lines <- readPuzzleInput filename
  let seats = map readPass lines
      largestId = maximum seats
      missingId = findMissing seats
  print missingId
  print $ sort (map seatId seats)
