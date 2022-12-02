import System.IO

data Shape = Rock | Paper | Scissors deriving Show

data Outcome = Lose | Draw | Win

shapeScore Rock     = 1 
shapeScore Paper    = 2 
shapeScore Scissors = 3 

outcomeScore Lose = 0
outcomeScore Draw = 3
outcomeScore Win  = 6

theirShape 'A' = Rock
theirShape 'B' = Paper
theirShape 'C' = Scissors

myShape 'X' = Rock
myShape 'Y' = Paper
myShape 'Z' = Scissors

whatToForce 'X' = Lose
whatToForce 'Y' = Draw
whatToForce 'Z' = Win

--      theirs   mine
outcome Rock     Scissors = Lose
outcome Paper    Rock     = Lose
outcome Scissors Paper    = Lose
outcome Rock     Paper    = Win
outcome Paper    Scissors = Win
outcome Scissors Rock     = Win
outcome _        _        = Draw

force Win  Rock     = Paper
force Lose Rock     = Scissors
force Win  Paper    = Scissors
force Lose Paper    = Rock
force Win  Scissors = Rock
force Lose Scissors = Paper
force Draw s        = s

gamePoints theirs mine = shapeScore mine + outcomeScore (outcome theirs mine)

part1 games = sum $ map (uncurry gamePoints) games

part2 games = sum $ map f games
  where f (theirs, result) = gamePoints theirs (force result theirs)

main = do
  content <- readFile "02.txt"
  let contentLines = filter (not . null) $ lines content
  let linePairs = [ (line !! 0, line !! 2 ) | line <- contentLines ]
  let part1Info = [ (theirShape t, myShape m)     | (t, m) <- linePairs ]
  let part2Info = [ (theirShape t, whatToForce m) | (t, m) <- linePairs ]

  print $ part1 part1Info
  print $ part2 part2Info
