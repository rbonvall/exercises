import System.IO
import Data.Sequence (Seq, index, update, fromList)
import Data.Maybe (isJust)

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

part1 ops =
  let restoredOps = (update 1 12 . update 2 2) ops
      s0 = State 0 restoredOps
      ss = takeWhile isJust $ iterate (next =<<) (Just s0)
      sf = last $ takeWhile isJust ss
  in valueInPositionZero sf
  where valueInPositionZero (Just (State _ ops)) = index ops 0

main = do
  handle <- openFile "02.txt" ReadMode
  contents <- hGetContents handle
  let opList = read ("[" ++ contents ++ "]") :: [Int]
  print $ part1 (fromList opList)
  hClose handle
