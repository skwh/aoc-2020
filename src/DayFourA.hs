-- DayFourA.hs
module DayFourA (main) where

import qualified Text.Parsec as Parsec
import Text.Parsec (try)
import Text.Parsec.Error (ParseError(..))
import Control.Monad.Identity (Identity)
import Control.Applicative
import Data.Either (rights)

data PassportField = BirthYear Int    |
                     IssueYear Int    |
                     ExpYear Int      |
                     Height Int       |
                     HairColor String |
                     EyeColor String  |
                     PassportId Int   |
                     CountryId Int    deriving (Eq, Show)

type Passport = [PassportField]

birthYearP :: Parsec.Parsec String () PassportField
birthYearP = do
  Parsec.string "byr"
  Parsec.char ':'
  yr <- read <$> Parsec.count 4 Parsec.digit
  if yr <= 1920 || yr >= 2002
  then Parsec.unexpected "byr out of range"
  else return (BirthYear yr)

issueYearP :: Parsec.Parsec String () PassportField
issueYearP = do
  Parsec.string "iyr"
  Parsec.char ':'
  yr <- read <$> Parsec.count 4 Parsec.digit
  if yr <= 2010 || yr >= 2020
  then Parsec.unexpected "iyr out of range"
  else return (IssueYear yr)

expYearP :: Parsec.Parsec String () PassportField
expYearP = do
  Parsec.string "eyr"
  Parsec.char ':'
  yr <- read <$> Parsec.count 4 Parsec.digit
  if yr <= 2020 || yr >= 2030
  then Parsec.unexpected "eyr out of range"
  else return (ExpYear yr)

heightP :: Parsec.Parsec String () PassportField
heightP = do
  Parsec.string "hgt"
  Parsec.char ':'
  hgt <- read <$> Parsec.many1 Parsec.digit
  unit <- Parsec.string "in" <|> Parsec.string "cm"
  return (Height hgt)

hairColorP :: Parsec.Parsec String () PassportField
hairColorP = do
  Parsec.string "hcl"
  Parsec.char ':' 
  Parsec.char '#'
  clr <- Parsec.many1 Parsec.hexDigit 
  return $ HairColor clr

eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

eyeColorP :: Parsec.Parsec String () PassportField
eyeColorP = do
  Parsec.string "ecl"
  Parsec.char ':'
  clr <- Parsec.many1 Parsec.letter
  if clr `notElem` eyeColors
  then Parsec.unexpected "invalid ecl"
  else return $ EyeColor clr

passIdP :: Parsec.Parsec String () PassportField
passIdP = do
  Parsec.string "pid"
  Parsec.char ':'
  id <- Parsec.many1 Parsec.digit
  return $ PassportId $ read id

countryIdP :: Parsec.Parsec String () PassportField
countryIdP = do
  Parsec.string "cid"
  Parsec.char ':'
  id <- read <$> Parsec.many1 Parsec.digit
  return $ CountryId id

passFieldP =   try eyeColorP
           <|> try expYearP
           <|> try hairColorP
           <|> try heightP
           <|> try passIdP
           <|> try countryIdP
           <|> try birthYearP
           <|> try issueYearP

passportP = Parsec.endBy passFieldP (try Parsec.endOfLine <|> try Parsec.space)

passportsP :: Parsec.Parsec String () [Passport]
passportsP = Parsec.sepBy passportP Parsec.endOfLine

inputFileP = do
  ps <- passportsP
  Parsec.eof
  return ps

filename = "input/day4-input.txt"

parseInputFile :: String -> Either Parsec.ParseError [Passport]
parseInputFile = Parsec.parse inputFileP "(source)"

main :: IO ()
main = do
  file <- readFile filename
  let rsl = parseInputFile file
  print rsl
