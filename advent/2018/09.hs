import System.IO

data Game = Game { players :: Int, lastMarbePoints :: Int }

input    =   Game 405 70953
examples = [ Game  10  1618
           , Game  13  7999
           , Game  17  1104
           , Game  21  6111
           , Game  30  5807
           ]

part1 (Game ps pts) = ""

part2 (Game ps pts) = ""

main = do
  print $ part1 input
  print $ part2 input


