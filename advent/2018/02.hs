import qualified Data.Map as Map
import Data.List (sort)
import System.IO

count xs = foldl updateCount Map.empty xs
  where updateCount acc x = Map.insertWith (+) x 1 acc

hasTwoAndThree = foldl f (False, False)
  where f (has2, has3) n = (has2 || n == 2, has3 || n == 3)

commonLetters v w = map fst $ filter areEqual $ zip v w
  where areEqual (x, y) = x == y

part1 ids =
  let c = foldl f (0, 0) $ map (hasTwoAndThree . count) ids
  in  fst c * snd c
  where
    toInt b = if b then 1 else 0
    f (c2, c3) (has2, has3) = (c2 + toInt has2, c3 + toInt has3)

part2 ids =
  let sortedPairs        = pairs $ sort $ ids
      sortedShiftedPairs = pairs $ sort $ map shift ids
      allSortedPairs     = sortedPairs ++ sortedShiftedPairs
      commonParts        = map (uncurry commonLetters) allSortedPairs
  in  filter isLongest commonParts
  where
    shift (x : xs) = xs ++ [x]
    pairs l@(_ : xs) = zip l xs
    n = length $ head ids
    isLongest w = length w == n - 1

main = do
  handle <- openFile "02.txt" ReadMode
  contents <- hGetContents handle
  let ids = lines contents
  print $ part1 ids
  print $ part2 ids
  hClose handle

