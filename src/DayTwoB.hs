-- DayTwoA.hs
module DayTwoA (main) where

import qualified Text.Parsec as Parsec
import Control.Monad.Identity (Identity)
import Data.Either (rights)

type Policy = (Int, Int, Char)
type PasswordLine = (Policy, String)

blankLine = ((0, 0, ' '), "")
lineregex = "([1-9]+)-([1-9]+) ([a-z]): ([a-z]+)"

-- parser

dashCharP = Parsec.char '-'
colCharP = Parsec.char ':'

passwordLineParser :: Parsec.Parsec String () PasswordLine
passwordLineParser = do
  low <- Parsec.many1 Parsec.digit
  dashCharP
  high <- Parsec.many1 Parsec.digit
  Parsec.spaces
  chr <- Parsec.letter
  colCharP
  Parsec.spaces
  str <- Parsec.many1 Parsec.letter
  return ((read low, read high, chr), str)

parsePasswordLine :: String -> Either Parsec.ParseError PasswordLine
parsePasswordLine = Parsec.parse passwordLineParser "(source)"

readPasswordLines :: [String] -> [PasswordLine]
readPasswordLines = rights.map parsePasswordLine 

readPasswordFile :: String -> IO [PasswordLine]
readPasswordFile filename = do
  file <- readFile filename
  return $ readPasswordLines $ lines file 

filename = "input/day2-input.txt"

-- isPasswordValid :: Policy -> String -> Bool
-- isPasswordValid policy str = valid' policy 0 str
--  where valid' (min, max, c) count [] = count <= max && count >= min
--        valid' (min, max, c) count (x:xs) = if c == x
--                                            then valid' (min, max, c) (count + 1) xs
--                                            else valid' (min, max, c) count xs

isPasswordValid :: Policy -> String -> Bool
isPasswordValid (idxa, idxb, chr) str = xor idxaMatch idxbMatch
  where idxaMatch = str !! (idxa - 1) == chr
        idxbMatch = str !! (idxb - 1) == chr
        xor a b = (a && not b) || (not a && b)


count :: Eq a => a -> [a] -> Int
count v vs = c' v vs 0
  where c' v [] num = num
        c' v (x:xs) num = if v == x
                          then c' v xs (num + 1)
                          else c' v xs num

main :: IO ()
main = do
  lines <- readPasswordFile filename
  print $ length lines 
  print $ count True $ map (uncurry isPasswordValid) lines
