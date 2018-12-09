import System.IO

parseDependency :: String -> (Char, Char)
parseDependency line =
  let ws = map head $ words line
  in (ws !! 1, ws !! 7)

part1 _ = ""

part2 _ = ""

example = [ "Step C must be finished before step A can begin."
          , "Step C must be finished before step F can begin."
          , "Step A must be finished before step B can begin."
          , "Step A must be finished before step D can begin."
          , "Step B must be finished before step E can begin."
          , "Step D must be finished before step E can begin."
          , "Step F must be finished before step E can begin."
          ]
ds = map parseDependency example

main = do
  handle <- openFile "07.txt" ReadMode
  contents <- hGetContents handle
  let dependencies = map parseDependency $ lines contents
  print $ dependencies
  print $ part1 dependencies
  print $ part2 dependencies
  hClose handle

