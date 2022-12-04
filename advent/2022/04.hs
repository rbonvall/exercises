import System.IO

data Range = Range Int Int

ordered x y z = x <= y && y <= z

fullyContains (Range a b) (Range c d) = ordered a c b && ordered a d b

overlap (Range a b) (Range c d) = a <= d && c <= b

part1 = length . filter oneFullyContainsTheOther
  where oneFullyContainsTheOther (r1, r2) = r1 `fullyContains` r2 || r2 `fullyContains` r1

part2 = length . filter (uncurry overlap)

split2 sep xs =
  let (left, right) = break (==sep) xs
  in (left, tail right)

parseLine line =
  let (l, r) = split2 ',' line
      (l1, l2) = split2 '-' l
      (r1, r2) = split2 '-' r
      fstRange = Range (read l1 :: Int) (read l2 :: Int)
      sndRange = Range (read r1 :: Int) (read r2 :: Int)
  in (fstRange, sndRange)

main = do
  content <- readFile "04.txt"
  let input = map parseLine $ filter (not . null) $ lines content

  print $ part1 input
  print $ part2 input

