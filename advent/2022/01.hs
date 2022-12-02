import System.IO
import Data.List (sort)

groups [] = []
groups ("" : ss) = groups ss
groups ls =
  let (firstGroup, rest) = break null ls
  in firstGroup : groups rest


part1 gs = maximum $ map sum gs

part2 gs = sum $ take 3 $ reverse $ sort $ map sum gs

main = do
  contents <- readFile "01.txt"
  let paragraphs = groups $ lines contents
  let elfCalorieGroups = map (map read) paragraphs :: [[Int]]

  print $ part1 elfCalorieGroups
  print $ part2 elfCalorieGroups
