import Data.Char (toLower, isLower, isLetter)
import System.IO

react (u : v : rest)
  | destroy u v = react rest
  | otherwise   = u : react (v : rest)
  where destroy u v = toLower u == toLower v && isLower u /= isLower v
react (u : []) = u : []
react [] = []

firstRepeated (x : y : rest)
  | x == y    = x
  | otherwise = firstRepeated (y : rest)

converge f = firstRepeated . (iterate f)

part1 = length . (converge react)

part2 polymer = minimum $ map part1 $ map (removeFrom polymer) ['a'..'z']
  where
    removeFrom string letter = filter (isnt letter) string
    isnt c d = toLower c /= toLower d

example = "dabAcCaCBAcCcaDA"

main = do
  handle <- openFile "05.txt" ReadMode
  contents <- hGetContents handle
  let polymer = filter isLetter contents
  print $ part1 polymer
  print $ part2 polymer
  hClose handle

