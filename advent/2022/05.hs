import System.IO (print, readFile)
import Data.List (isPrefixOf, transpose)
import Data.Char (isLetter, isSpace)

data Move = Move
    { howMany  :: Int
    , moveFrom :: Int
    , moveTo   :: Int
    }

parseMoveLine line =
  let ws = words line
      m = read (ws !! 1) :: Int
      f = read (ws !! 3) :: Int
      t = read (ws !! 5) :: Int
  in Move m f t

crateMover9000 = reverse
crateMover9001 = id

applyMove machine stacks (Move m from to) =
  let toMove = machine $ take m (stacks !! (from - 1))
      f stack i =
        if      i == from then drop m stack
        else if i == to   then toMove ++ stack
        else                   stack
  in [ f stack i | (stack, i) <- zip stacks [1 ..] ]

solveForMachine machine stacks moves =
  map head $ foldl (applyMove machine) stacks moves

part1 = solveForMachine crateMover9000
part2 = solveForMachine crateMover9001

main = do
  content <- readFile "05.txt"
  let contentLines = lines content

  let moveLines = filter ("move" `isPrefixOf`) contentLines
  let moves = map parseMoveLine moveLines

  let crateStackLines = takeWhile (elem '[') contentLines
  let crateStackColumns = filter (any isLetter) $ transpose crateStackLines
  let crateStacks = map (dropWhile isSpace) crateStackColumns

  print $ part1 crateStacks moves
  print $ part2 crateStacks moves

