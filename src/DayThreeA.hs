-- DayThreeA.hs
module DayThreeA (main) where

type SledMap = [String]

filename = "input/day3-input.txt"

readInputFile :: String -> IO [String]
readInputFile path = lines <$> readFile path

makeMap :: [String] -> SledMap
makeMap = map cycle  

isTree :: Char -> Bool
isTree = (==) '#'

traverseMapWithSlope :: SledMap -> (Int, Int) -> Int
traverseMapWithSlope map pair = traverse map pair 0 0
  where traverse :: SledMap -> (Int, Int) -> Int -> Int -> Int
        traverse [] (dx, dy) mx count = count
        traverse ls (dx, dy) mx count = if dy >= length ls
                                        then count
                                        else traverse' ls (dx, dy) mx count
        traverse' ls (dx, dy) mx count = let restLs@(a:as) = drop dy ls
                                             nmx = dx + mx
                                             c = head $ drop nmx a
                                         in if isTree c
                                            then traverse restLs (dx, dy) nmx (count+1)
                                            else traverse restLs (dx, dy) nmx count


main :: IO ()
main = do
  sledMap <- makeMap <$> readInputFile filename
  print $ traverseMapWithSlope sledMap (3, 1)
