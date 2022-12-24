import System.IO (print, readFile, putStrLn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Side = XY (Int, Int) Int   -- (x0, y0) z
          | XZ (Int, Int) Int   -- (x0, z0) y
          | YZ (Int, Int) Int   -- (y0, z0) x
          deriving (Show, Eq, Ord)

cubeSides (x, y, z) =
  [ XY (x, y) z, XY (x, y) (z + 1)
  , XZ (x, z) y, XZ (x, z) (y + 1)
  , YZ (y, z) x, YZ (y, z) (x + 1)
  ]

countItems xs = foldl addToCounter Map.empty xs
  where addToCounter c x = Map.insertWith (+) x 1 c

fillInAirPockets x = x  -- TODO: implement

part1 cubes =
  let sides = concatMap cubeSides cubes
      counter = countItems sides
      nonConnected = Map.filter (==1) counter
  in Map.size nonConnected

part2 cubes =
  part1 $ fillInAirPockets cubes

parseLine :: String -> (Int, Int, Int)
parseLine line =
  let [x, y, z] = words [if c == ',' then ' ' else c | c <- line ]
  in (read x, read y, read z)


main = do
  contents <- readFile "18.txt"
  let input = [ parseLine line | line <- lines contents ]

  print $ part1 input
  print $ part2 input


