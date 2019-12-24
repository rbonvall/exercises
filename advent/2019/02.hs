import System.IO
import Data.Sequence (Seq, index, update, fromList)
import Data.Maybe (isJust)
import Data.List (find)

data State = State Int (Seq Int) deriving Show

example :: Seq Int
example = fromList [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]

sx = State 0 example

next :: State -> Maybe State
next (State pos ops) =
  let w = index ops pos
      x = index ops $ index ops (pos + 1)
      y = index ops $ index ops (pos + 2)
      z = index ops (pos + 3)
  in execute ops w x y z
  where 
    execute ops  1 x y z = Just (State (pos + 4) (update z (x + y) ops))
    execute ops  2 x y z = Just (State (pos + 4) (update z (x * y) ops))
    execute ops 99 _ _ _ = Nothing

process :: Int -> Int -> Seq Int -> Int
process noun verb ops =
  let restoredOps = (update 1 noun . update 2 verb) ops
      s0 = State 0 restoredOps
      ss = takeWhile isJust $ iterate (next =<<) (Just s0)
      sf = last $ takeWhile isJust ss
  in valueInPositionZero sf
  where valueInPositionZero (Just (State _ ops)) = index ops 0

part1 = process 12 2

part2 ops =
  let pairs = [ (noun, verb) | noun <- [0..99], verb <- [0..99] ]
      mappings = map tabulate pairs
  in fmap fst $ find ((==19690720) . snd) mappings
  where tabulate (n, v) = (100 * n + v, process n v ops)  

main = do
  contents <- readFile "02.txt"
  let opList = read ("[" ++ contents ++ "]") :: [Int]
  let opSeq = fromList opList
  print $ part1 opSeq
  print $ part2 opSeq
