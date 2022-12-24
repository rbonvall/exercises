import System.IO (print, readFile)

data Instruction = Noop | Addx Int deriving Show

data Cpu = Cpu { regX :: Int } deriving Show

executeOp (Cpu x) Noop     = Cpu x
executeOp (Cpu x) (Addx v) = Cpu (x + v)

-- cheat by inserting a Noop before every Addx to account for the extra cycle
cpuStates cpu ops = scanl executeOp cpu (f ops)
  where
    f (op:ops) =
      case op of
        Noop   ->      op:(f ops)
        Addx v -> Noop:op:(f ops)
    f [] = []

parseInstruction line =
  case words line of
    ["noop"]    -> Noop
    ["addx", v] -> Addx (read v)

part1 ops =
  let ss = cpuStates (Cpu 1) ops
      cycles = [20, 60, 100, 140, 180, 220]
      strengths = map (regX . (ss !!) . pred) cycles
  in sum $ zipWith (*) cycles strengths

part2 ops = ()

main = do
  contents <- readFile "10.txt"
  let ops = map parseInstruction $ filter (not . null) $ lines contents

  print $ part1 ops
  print $ part2 ops


