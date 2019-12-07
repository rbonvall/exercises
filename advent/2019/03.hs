import Data.Char (isDigit)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import System.IO

data Direction = U | D | L | R deriving Show

data Move = Move Direction Int deriving Show

instance Read Move where
  readsPrec _ (d : cs) =
    let (number, rest) = span isDigit cs
    in  [(Move (charToDir d) (read number :: Int), rest)]
    where
      charToDir 'U' = U
      charToDir 'D' = D
      charToDir 'L' = L
      charToDir 'R' = R
  readsPrec _ [] = []

withX x y = (x, y)
withY y x = (x, y)

segment :: (Int, Int) -> Move -> [(Int, Int)]
segment (x0, y0) (Move U n) = map (withX x0) [y0 + 1, y0 + 2 .. y0 + n]
segment (x0, y0) (Move D n) = map (withX x0) [y0 - 1, y0 - 2 .. y0 - n]
segment (x0, y0) (Move L n) = map (withY y0) [x0 - 1, x0 - 2 .. x0 - n]
segment (x0, y0) (Move R n) = map (withY y0) [x0 + 1, x0 + 2 .. x0 + n]

path :: [Move] -> [(Int, Int)]
path moves = path' moves [(0, 0)]
  where
    path' []     acc = acc
    path' (m:ms) acc = path' ms (acc ++ segment (last acc) m)
    -- inefficient: gets last element of linked list

parseWire :: String -> [Move]
parseWire line = read ("[" ++ line ++ "]")

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (x0, y0) (x1, y1) = abs (x1 - x0) + abs (y1 - y0)

stepMap :: [Move] -> Map.Map (Int, Int) Int
stepMap w =
  let p = tail $ path w
  in  Map.fromListWith min $ zip p [1..]

part1 :: [Move] -> [Move] -> Int
part1 w1 w2 =
  let s1 = Set.fromList (tail $ path w1)
      s2 = Set.fromList (tail $ path w2)
      crossings = Set.intersection s1 s2
  in minimum $ Set.map (manhattan (0, 0)) crossings

part2 :: [Move] -> [Move] -> Int
part2 w1 w2 =
  let sm1 = stepMap w1
      sm2 = stepMap w2
  in  minimum $ Map.intersectionWith (+) sm1 sm2

example1 = (parseWire "R8,U5,L5,D3",
            parseWire "U7,R6,D4,L4")
example2 = (parseWire "R75,D30,R83,U83,L12,D49,R71,U7,L72",
            parseWire "U62,R66,U55,R34,D71,R55,D58,R83")
example3 = (parseWire "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
            parseWire "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")
example4 = (parseWire "R3,U7,L1,D2,L3,D6,R3,U3,R6",
            parseWire "U5,R4,D1,L8,D6,R7,D1,R2,U6")

printExample e = do
  print $ uncurry part1 $ e
  print $ uncurry part2 $ e

main = do
  putStrLn "Example 1"
  printExample example1

  putStrLn "Example 2"
  printExample example2

  putStrLn "Example 3"
  printExample example3

  putStrLn "Example 4"
  printExample example4

  handle <- openFile "03.txt" ReadMode
  contents <- hGetContents handle
  let [w1, w2] = map parseWire $ lines contents

  putStrLn "Problem input"
  print $ part1 w1 w2
  print $ part2 w1 w2

  hClose handle
