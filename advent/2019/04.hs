import Data.Char (digitToInt)

digits :: Int -> [Int]
digits = map digitToInt . show

nonDecreasing :: [Int] -> Bool
nonDecreasing (x : y : rest) = x <= y && nonDecreasing (y : rest)
nonDecreasing _ = True

hasPair :: [Int] -> Bool
hasPair (x : y : rest) = x == y || hasPair (y : rest)
hasPair _ = False

hasExclusivePair :: [Int] -> Bool
hasExclusivePair ds@(x : y : rest)
  | x /= y    = hasExclusivePair (y : rest)
  | otherwise =
      let (xs, nonXs) = span (==x) ds
      in  length xs == 2 || hasExclusivePair nonXs
hasExclusivePair _ = False

candidates :: [[Int]]
candidates = map digits [235741 .. 706948]

nonDecreasingCandidates :: [[Int]]
nonDecreasingCandidates = filter nonDecreasing candidates

main = do
  print $ length $ filter hasPair          nonDecreasingCandidates
  print $ length $ filter hasExclusivePair nonDecreasingCandidates
