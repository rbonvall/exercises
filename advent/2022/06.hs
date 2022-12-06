import System.IO (print, readFile)
import Data.List (group, sort)

allDifferent = all (==1) . map length . group . sort

charsBeforeGroupOfDistinct n xs = f n xs
  where f c s@(_:xs) | allDifferent $ take n s = c
                     | otherwise               = f (c + 1) xs

part1 = charsBeforeGroupOfDistinct 4
part2 = charsBeforeGroupOfDistinct 14

main = do
  signal <- readFile "06.txt"
  print $ part1 signal
  print $ part2 signal

