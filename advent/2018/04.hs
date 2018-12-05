import Data.List (sort, groupBy)
import System.IO

data Event = Begins Int
           | WakesUp
           | FallsAsleep
           deriving (Show, Eq)

data Timestamp = Timestamp { year :: Int
                           , month :: Int
                           , day :: Int
                           , hours :: Int
                           , minutes :: Int
                           } deriving (Show, Eq, Ord)

data Record = Record Timestamp Event deriving (Show, Eq)

instance Ord Record where
  Record t1 _ <= Record t2 _ = t1 <= t2

data ShiftInfo = ShiftInfo Int [(Timestamp, Timestamp)] deriving (Show, Eq)

toInt s = read s :: Int

parseTimestamp :: String -> Timestamp
parseTimestamp s =
  let nums = map toInt $ words $ map desymbolize s
      y  = nums !! 0
      m  = nums !! 1
      d  = nums !! 2
      hh = nums !! 3
      mm = nums !! 4
  in Timestamp y m d hh mm
  where
    desymbolize c = if elem c ['0'..'9'] then c else ' '

parseEvent :: String -> Event
parseEvent line = p $ words line
  where
    p ("Guard" : ('#' : n) : "begins" : "shift" : []) = Begins $ toInt n
    p ("falls" : "asleep"                       : []) = FallsAsleep
    p ("wakes" : "up"                           : []) = WakesUp

tomorrow :: (Int, Int, Int) -> (Int, Int, Int)
tomorrow (y, 12, 31) = (y + 1, 1, 1)
tomorrow (y, m, d)
  | d == monthDays m = (y, m + 1, 1)
  | otherwise        = (y, m, d + 1)
  where monthDays = ([0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] !!)
-- data has no leap days

nextMidnight :: Timestamp -> Timestamp
nextMidnight (Timestamp y m d _ _) = Timestamp y' m' d' 0 0
  where (y', m', d') = tomorrow (y, m, d)

parseRecord :: String -> Record
parseRecord line =
  let (t, e) = splitAt 19 line
      timestamp = parseTimestamp t
      event     = parseEvent     e
  in Record timestamp event

example = [ "[1518-11-01 00:00] Guard #10 begins shift"
          , "[1518-11-01 00:05] falls asleep"
          , "[1518-11-01 00:25] wakes up"
          , "[1518-11-01 00:30] falls asleep"
          , "[1518-11-01 00:55] wakes up"
          , "[1518-11-01 23:58] Guard #99 begins shift"
          , "[1518-11-02 00:40] falls asleep"
          , "[1518-11-02 00:50] wakes up"
          , "[1518-11-03 00:05] Guard #10 begins shift"
          , "[1518-11-03 00:24] falls asleep"
          , "[1518-11-03 00:29] wakes up"
          , "[1518-11-04 00:02] Guard #99 begins shift"
          , "[1518-11-04 00:36] falls asleep"
          , "[1518-11-04 00:46] wakes up"
          , "[1518-11-05 00:03] Guard #99 begins shift"
          , "[1518-11-05 00:45] falls asleep"
          , "[1518-11-05 00:55] wakes up"
          ]
rs = map parseRecord example

pairs :: [a] -> [(a, a)]
pairs (x1 : x2 : xs) = (x1, x2) : pairs xs
pairs (_       : []) = []
pairs            []  = []

shiftInfoFromRecords :: [Record] -> ShiftInfo
shiftInfoFromRecords rs =
  let ((Record actualStartTime (Begins guard)) : rest) = rs
      start = roundUp actualStartTime
      asleepIntervals = []
  in ShiftInfo guard asleepIntervals
  where
    roundUp ts = if minutes ts == 23 then nextMidnight ts else ts


part1 records =
  let groups = groupBy shiftEnd records
      shifts = map shiftInfoFromRecords groups
  in  shifts
  where
    shiftEnd :: Record -> Record -> Bool
    shiftEnd _ (Record _ (Begins _)) = False
    shiftEnd _ _                     = True


part2 records = ""

main = do
  handle <- openFile "04.txt" ReadMode
  contents <- hGetContents handle
  let records = sort $ map parseRecord $ lines contents
  print $ part1 records
  print $ part2 records
  hClose handle

