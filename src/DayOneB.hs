-- DayOneB.hs
module DayOneB (main) where


import DayOneA (readReport)

filename = "day1-input.txt"

main :: IO ()
main = do
  nums <- readReport filename
  let triplets = [ (a, b, c) | a <- nums, b <- nums, c <- nums, (a + b + c) == 2020 ]
  printTriplet $ head triplets

printTriplet :: (Int, Int, Int) -> IO ()
printTriplet (a, b, c) = print (a * b * c)
