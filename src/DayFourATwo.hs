-- DayFourATwo.hs
module DayFourATwo (main) where

import qualified Text.Parsec as P
import Text.Parsec (try)
import Control.Monad.Identity (Identity)
import Control.Applicative
import Data.Either (fromRight)

data PassportField = BirthYear |
                     IssueYear |
                     ExpirationYear |
                     Height |
                     PassportId |
                     HairColor |
                     EyeColor |
                     CountryId deriving (Eq, Show)

type Passport = [PassportField]

passportCharacterP = P.alphaNum <|> P.hexDigit <|> P.char '#'

birthYearP :: P.Parsec String () PassportField
birthYearP = do
  P.string "byr"
  P.char ':'
  P.many1 passportCharacterP
  return BirthYear

issueYearP :: P.Parsec String () PassportField
issueYearP = do
  P.string "iyr"
  P.char ':'
  P.many1 passportCharacterP
  return IssueYear

expirationYearP :: P.Parsec String () PassportField
expirationYearP = do
  P.string "eyr"
  P.char ':'
  P.many1 passportCharacterP
  return ExpirationYear

eyeColorP :: P.Parsec String () PassportField
eyeColorP = do
  P.string "ecl"
  P.char ':'
  P.many1 passportCharacterP
  return EyeColor

passportIdP :: P.Parsec String () PassportField
passportIdP = do
  P.string "pid"
  P.char ':'
  P.many1 passportCharacterP
  return PassportId

countryIdP :: P.Parsec String () PassportField
countryIdP = do
  P.string "cid"
  P.char ':'
  P.many1 passportCharacterP
  return CountryId

hairColorP :: P.Parsec String () PassportField
hairColorP = do
  P.string "hcl"
  P.char ':'
  P.many1 passportCharacterP
  return HairColor

heightP :: P.Parsec String () PassportField
heightP = do
  P.string "hgt"
  P.char ':'
  P.many1 passportCharacterP
  return Height

passportFieldP =   try birthYearP
               <|> try issueYearP
               <|> try expirationYearP
               <|> try eyeColorP
               <|> try passportIdP
               <|> try hairColorP
               <|> try countryIdP
               <|> try heightP

passportP = P.endBy passportFieldP (try P.endOfLine <|> try P.space)

passportsP = P.sepBy passportP P.endOfLine

inputFileP = do
  ps <- passportsP
  P.eof
  return ps

filename = "input/day4-input.txt"

passportValid ps =  BirthYear `elem` ps
                 && IssueYear `elem` ps
                 && ExpirationYear `elem` ps
                 && EyeColor `elem` ps
                 && HairColor `elem` ps
                 && Height `elem` ps
                 && PassportId `elem` ps

count :: Ord a => [a] -> a -> Int
count as a = c' as a 0
  where c' (b:as) a c = if b == a
                        then c' as a (c+1)
                        else c' as a c
        c' [] a c = c

main :: IO ()
main = do
  file <- readFile filename
  let passports = fromRight [] $ P.parse inputFileP "(source)" file
      valids = map passportValid passports
      pcount = count valids True
  print passports
  print pcount

