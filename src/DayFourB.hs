-- DayFourB.hs
{-# LANGUAGE RecordWildCards #-}

module DayFourB (main) where

import qualified Text.Parsec as P
import Text.Parsec (try)
import Control.Monad.Identity (Identity)
import Control.Applicative
import Data.List (partition)
import Data.Either (fromRight)
import Data.Char (isDigit, isHexDigit)

data PassportField = BirthYear String |
                     IssueYear String |
                     ExpirationYear String |
                     Height String |
                     PassportId String |
                     HairColor String |
                     EyeColor String |
                     CountryId String deriving (Eq, Show)

type Passport = [PassportField]

data PassportFieldCounter = Counter { birthYear :: !Int
                                    , issueYear :: !Int
                                    , expirationYear :: !Int
                                    , height :: !Int
                                    , passportId :: !Int
                                    , hairColor :: !Int
                                    , eyeColor :: !Int
                                    , countryId :: !Int } deriving (Show)

emptyCounter :: PassportFieldCounter
emptyCounter = Counter 0 0 0 0 0 0 0 0 

counterInvalid :: PassportFieldCounter -> Bool
counterInvalid Counter{..} = birthYear > 1 || issueYear > 1 || expirationYear > 1 || height > 1 || passportId > 1 || hairColor > 1 || eyeColor > 1 || countryId > 1

countField :: PassportFieldCounter -> PassportField -> PassportFieldCounter
countField Counter{..} field = case field
                                 of BirthYear _ -> Counter {birthYear=birthYear+1,..}
                                    IssueYear _ -> Counter {issueYear=issueYear+1,..}
                                    ExpirationYear _ -> Counter{expirationYear=expirationYear+1,..}
                                    Height _ -> Counter{height=height+1,..}
                                    PassportId _ -> Counter{passportId=passportId+1,..}
                                    HairColor _ -> Counter{hairColor=hairColor+1,..}
                                    EyeColor _ -> Counter{eyeColor=eyeColor+1,..}
                                    CountryId _ -> Counter{countryId=countryId+1,..}

countFields :: Passport -> PassportFieldCounter
countFields = foldl countField emptyCounter

passportCharacterP = P.alphaNum <|> P.hexDigit <|> P.char '#'

birthYearP :: P.Parsec String () PassportField
birthYearP = do
  P.string "byr"
  P.char ':'
  val <- P.many1 passportCharacterP
  return $ BirthYear val

issueYearP :: P.Parsec String () PassportField
issueYearP = do
  P.string "iyr"
  P.char ':'
  val <- P.many1 passportCharacterP
  return $ IssueYear val

expirationYearP :: P.Parsec String () PassportField
expirationYearP = do
  P.string "eyr"
  P.char ':'
  val <- P.many1 passportCharacterP
  return $ ExpirationYear val

eyeColorP :: P.Parsec String () PassportField
eyeColorP = do
  P.string "ecl"
  P.char ':'
  val <- P.many1 passportCharacterP
  return $ EyeColor val

passportIdP :: P.Parsec String () PassportField
passportIdP = do
  P.string "pid"
  P.char ':'
  val <- P.many1 passportCharacterP
  return $ PassportId val

countryIdP :: P.Parsec String () PassportField
countryIdP = do
  P.string "cid"
  P.char ':'
  val <- P.many1 passportCharacterP
  return $ CountryId val

hairColorP :: P.Parsec String () PassportField
hairColorP = do
  P.string "hcl"
  P.char ':'
  val <- P.many1 passportCharacterP
  return $ HairColor val

heightP :: P.Parsec String () PassportField
heightP = do
  P.string "hgt"
  P.char ':'
  val <- P.many1 passportCharacterP
  return $ Height val

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

numFieldValid len min max str = length str == len && num >= min && num <= max
  where num = read str

birthYearValid = numFieldValid 4 1920 2002
issueYearValid = numFieldValid 4 2010 2020
expYearValid = numFieldValid 4 2020 2030

heightValid str = let cmLen = 3
                      inLen = 2
                      (num, unit) = span isDigit str
                  in if unit == "in"
                     then numFieldValid inLen 59 76 num
                     else unit == "cm" && numFieldValid cmLen 150 193 num

hclValid (a:as) = a == '#' && all isHexDigit as && length as == 6 

validEcls = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
eclValid = flip elem validEcls

pidValid str = (length str == 9) && all isDigit str
cidValid = True

fieldValid :: PassportField -> Bool
fieldValid f = case f
                 of (BirthYear v) -> birthYearValid v
                    (IssueYear v) -> issueYearValid v
                    (ExpirationYear v) -> expYearValid v
                    (Height v) -> heightValid v
                    (HairColor v) -> hclValid v
                    (EyeColor v) -> eclValid v
                    (PassportId v) -> pidValid v
                    (CountryId v) -> cidValid 

passportValid :: Passport -> Bool
passportValid passport = pass && length passport >= 7
  where pass = all (True ==) $ map fieldValid passport

showValidation [] = print "Done"
showValidation (p:ps) = do
  print p
  print $ fieldValid p
  showValidation ps

count :: Eq a => [a] -> a -> Int
count as a = c' as a 0
  where c' (b:as) a c = if b == a
                        then c' as a (c+1)
                        else c' as a c
        c' [] a c = c

countValids :: Passport -> String
countValids passport = show vcount ++ "/" ++ show (length passport)
  where fieldValids = map fieldValid passport
        vcount = count fieldValids True

getPassports file = fromRight [] $ P.parse inputFileP "(source)" file

main :: IO ()
main = do
  file <- readFile filename
  let passports = getPassports file
      valids = map passportValid passports
      counts = map countValids passports
      pcount = length.fst $ partition (== True) valids
      fcounts = map countFields passports
      fcvalids = map counterInvalid fcounts
      fcinvalids = count fcvalids True
  print pcount


-- between 142 and 157
