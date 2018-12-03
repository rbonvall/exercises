import qualified Data.Map as Map
import System.IO

count xs = foldl updateCount Map.empty xs
  where updateCount acc x = Map.insertWith (+) x 1 acc

hasTwoAndThree = foldl f (False, False)
  where f (has2, has3) n = (has2 || n == 2, has3 || n == 3)

part1 ids = fst c * snd c
  where
    toInt b = if b then 1 else 0
    f (c2, c3) (has2, has3) = (c2 + toInt has2, c3 + toInt has3)
    c = foldl f (0, 0) $ map (hasTwoAndThree . count) ids

part2 ids = ""

main = do
  handle <- openFile "02.txt" ReadMode
  contents <- hGetContents handle
  let ids = lines contents
  print $ part1 ids
  print $ part2 ids
  hClose handle

