import System.IO
import Data.Sequence (Seq, index, update, fromList)
import Data.Maybe (isJust, catMaybes)
import Data.List (find)
import Control.Exception (assert)

data State = State { position   :: Int
                   , operations :: Seq Int
                   , inputs     :: [Int]
                   , outputs    :: [Int]
                   } deriving Show

data Mode = Pos | Imm deriving Show

parseInstruction n =
  let op = n `mod` 100
      m1 = toMode $ n `div` 100   `mod` 10
      m2 = toMode $ n `div` 1000  `mod` 10
      m3 = toMode $ n `div` 10000 `mod` 10
  in (op, m1, m2, m3)
  where
    toMode 0 = Pos
    toMode _ = Imm

next :: State -> Maybe State
next s@(State pos ops ins outs) =
  let (op, m1, m2, m3) = parseInstruction $ index ops pos
      x1 = getValue m1 ops (pos + 1)
      x2 = getValue m2 ops (pos + 2)
      x3 = getValue m3 ops (pos + 3)
  in execute ops op x1 x2 x3
  where
    getValue Pos ops n = (index ops . index ops) n
    getValue Imm ops n = index ops n
    execute ops  1 x y z = Just $ State (pos + 4) (update z (x + y)    ops) ins        outs
    execute ops  2 x y z = Just $ State (pos + 4) (update z (x * y)    ops) ins        outs
    execute ops  3 x _ _ = Just $ State (pos + 2) (update x (head ins) ops) (tail ins) outs
    execute ops  4 x _ _ = Just $ State (pos + 2) ops                       ins     (x:outs)
    execute ops 99 _ _ _ = Nothing
    execute _    _ _ _ _ = error $ show s

process :: Seq Int -> [Int] -> [Int]
process ops inputs =
  let s0 = State 0 ops inputs []
      ss = catMaybes $ takeWhile isJust $ iterate (next =<<) (Just s0)
  in outputs $ last ss

part1 ops = process ops [1]

main = do
  contents <- readFile "05.txt"
  let opList = read ("[" ++ contents ++ "]") :: [Int]
  let opSeq = fromList opList
  print $ part1 opSeq
  --print $ part2 opSeq
