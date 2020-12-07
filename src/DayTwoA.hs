-- DayTwoA.hs
module DayTwoA (main) where

type Policy = (Int, Int, Char)
type PasswordLine = (Policy, String)

blankLine = ((0, 0, ' '), "")

data PolicyValue = PolicyMin Int |
                   PolicyMax Int |
                   PolicyChar Char |
                   Password String deriving (Show, Eq)

data OtherValue = ValueChar Char |
                  ValueSpace deriving (Show, Eq)

newtype PolicyParser a = Parser { runParser :: String -> Maybe (String, a) }

instance PolicyParser Functor where

readPasswordFile :: String -> IO [PasswordLine]
readPasswordFile filename = do
  file <- readFile filename
  let filelines = lines file
  return $ map readPasswordLine filelines


filename = "day2-input.txt"

isPasswordValid :: Policy -> String -> Bool
isPasswordValid policy str = valid' policy 0 str
  where valid' (min, max, c) count [] = count < max && count > min
        valid' (min, max, c) count (x:xs) = if c == x
                                            then valid' (min, max, c) (count + 1) xs
                                            else valid' (min, max, c) count xs

count :: Eq a => a -> [a] -> Int
count v vs = c' v vs 0
  where c' v [] num = num
        c' v (x:xs) num = if v == x
                          then c' v xs (num + 1)
                          else c' v xs num

main :: IO ()
main = do
  lines <- readPasswordFile filename
  print $ count True $ map (uncurry isPasswordValid) lines
