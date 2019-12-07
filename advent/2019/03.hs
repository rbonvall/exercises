import Data.Char (isDigit)
import qualified Data.Set as Set
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

example1 = (parseWire "R8,U5,L5,D3",
            parseWire "U7,R6,D4,L4",
            6)
example2 = (parseWire "R75,D30,R83,U83,L12,D49,R71,U7,L72",
            parseWire "U62,R66,U55,R34,D71,R55,D58,R83",
            159)
example3 = (parseWire "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
            parseWire "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7",
            135)

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

intersections w1 w2 =
  let s1 = Set.fromList (path w1)
      s2 = Set.fromList (path w2)
  in Set.toList $ Set.intersection s1 s2

parseWire :: String -> [Move]
parseWire line = read ("[" ++ line ++ "]")

manhattan (x0, y0) (x1, y1) = abs (x1 - x0) + abs (y1 - y0)

part1 w1 w2 = minimum $ filter (>0) $ map (manhattan (0, 0)) $ intersections w1 w2

main = do
  handle <- openFile "03.txt" ReadMode
  contents <- hGetContents handle
  let [w1, w2] = map parseWire $ lines contents
  print $ part1 w1 w2
  hClose handle
