import System.IO (print, readFile, putStrLn)
import Data.Char (isDigit)
import Data.List (sort, nub)
import Data.Maybe (catMaybes)

-- both ends are inclusive
data Interval = Interval { left :: Int, right :: Int } deriving Eq

a~b = Interval a b

instance Show Interval where
  show (Interval a b) = (show a) ++ "~" ++ (show b)

ends (Interval a b) = [a, b]

disjoint (Interval a1 b1) (Interval a2 b2) = b1 < a2 || b2 < a1

-- Example: [0-3, 4-7, 9-10, 12-12, 13-15] -> [0-7, 9-10, 12-15]
mergeDisjointSortedIntervals ((i1@(Interval a1 b1)) : (i2@(Interval a2 b2)) : is) =
  if b1 == a2 - 1
  then mergeDisjointSortedIntervals ((Interval a1 b2):is)
  else i1 : (mergeDisjointSortedIntervals (i2:is))
mergeDisjointSortedIntervals [i] = [i]
mergeDisjointSortedIntervals []  = []

-- Example: [8, 2, 4, 6, 3, 0, 3, 6] -> [0~0, 2~4, 6~6, 8~8]
intervalsFromValues = mergeDisjointSortedIntervals . map singleton . sort . nub
  where singleton a = a~a

-- Insert an interval a~b into a list of sorted disjoint non-adjacent intervals,
-- given that a and b already are contained in some of those intervals.
-- Example: insert 6~10 into [2~3, 6~7, 9~12, 15~20] -> [2~3, 6~12, 15~20]
insertInterval is i@(Interval a b) =
  let (toTheLeft,   rest      ) = break (not . disjoint i) is
      (inTheMiddle, toTheRight) = break       (disjoint i) rest
      aNew = minimum (a:(map left  inTheMiddle))
      bNew = maximum (b:(map right inTheMiddle))
  in toTheLeft ++ [aNew~bNew] ++ toTheRight

mergeIntervals intervals =
  let initial = intervalsFromValues $ concatMap ends intervals
  in foldl insertInterval initial intervals

data SensorBeaconPair = SensorBeaconPair { sensor :: (Int, Int), beacon :: (Int, Int) } deriving Show

dist (x0, y0) (x1, y1) = abs (x1 - x0) + abs (y1 - y0)

intersectWithHorizontalStrip yStrip (SensorBeaconPair s@(xs, ys) b@(xb, yb)) =
  let distSensorToStrip = abs (yStrip - ys)
      radius = dist s b
      dx = radius - distSensorToStrip
      xLeft  = xs - dx
      xRight = xs + dx
  in 
    if      dx < 0                  then Nothing
    else if dx == 0 && yb == yStrip then Nothing
    else if (xLeft,  yStrip) == b   then Just $ (xLeft + 1) ~  xRight
    else if (xRight, yStrip) == b   then Just $  xLeft      ~ (xRight - 1)
    else                                 Just $  xLeft      ~  xRight     

nonBeaconPositions yStrip pairs = 
  let stripIntersections = catMaybes $ map (intersectWithHorizontalStrip yStrip) pairs
  in  mergeIntervals stripIntersections

y0 = 2000000 

part1 pairs = sum $ map intervalLength $ nonBeaconPositions y0 pairs
  where intervalLength (Interval a b) = b - a + 1

part2 pairs = ()
--  let nonPos = nonBeaconPositions y0 pairs
--      x = (right (head nonPos)) + 1
--  in tuningFrequency (x, y0)
--  where tuningFrequency (x, y) = 4000000 * x + y

example =
  [ SensorBeaconPair ( 2, 18)  (-2, 15)
  , SensorBeaconPair ( 9, 16)  (10, 16)
  , SensorBeaconPair (13,  2)  (15,  3)
  , SensorBeaconPair (12, 14)  (10, 16)
  , SensorBeaconPair (10, 20)  (10, 16)
  , SensorBeaconPair (14, 17)  (10, 16)
  , SensorBeaconPair ( 8,  7)  ( 2, 10)
  , SensorBeaconPair ( 2,  0)  ( 2, 10)
  , SensorBeaconPair ( 0, 11)  ( 2, 10)
  , SensorBeaconPair (20, 14)  (25, 17)
  , SensorBeaconPair (17, 20)  (21, 22)
  , SensorBeaconPair (16,  7)  (15,  3)
  , SensorBeaconPair (14,  3)  (15,  3)
  , SensorBeaconPair (20,  1)  (15,  3)
  ]

eis = [5~10, 1~3, 10~20, 11~14, 2~3, 11~13]

parseLine line = 
  let ws = words line
      sensorX = parseCoordinate (ws !! 2)
      sensorY = parseCoordinate (ws !! 3)
      beaconX = parseCoordinate (ws !! 8)
      beaconY = parseCoordinate (ws !! 9)
  in SensorBeaconPair (sensorX, sensorY) (beaconX, beaconY)
  where parseCoordinate s = read (filter isDigit s) :: Int

main = do
  contents <- readFile "15.txt"
  let input = [ parseLine line | line <- lines contents ]

  print $ part1 input
  print $ part2 input

