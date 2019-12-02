import System.IO

examples = [ 12, 14, 1969, 100756 ]

fuelNeeded m = (m `div` 3) - 2

part1 = sum . map fuelNeeded

part2 = \x -> 0

main = do
  handle <- openFile "01.txt" ReadMode
  contents <- hGetContents handle
  let moduleMasses = map (read :: String -> Int) (lines contents)
  print $ map fuelNeeded examples
  print $ part1 moduleMasses
  print $ part2 moduleMasses
  hClose handle

