import Data.List (group, sort)
import System.IO

data Claim = Claim { id     :: Int
                   , left   :: Int
                   , top    :: Int
                   , width  :: Int
                   , height :: Int
                   } deriving Show

parseClaim :: String -> Claim
parseClaim line =
  let nums = map toInt $ words $ map desymbolize line
      i = nums !! 0
      l = nums !! 1
      t = nums !! 2
      w = nums !! 3
      h = nums !! 4
  in Claim i l t w h
  where
    toInt n = read n :: Int
    desymbolize c = if elem c ['0'..'9'] then c else ' '

claimSquares :: Claim -> [(Int, Int)]
claimSquares (Claim _ l t w h) =
  [(i, j) | i <- [l .. l + w - 1]
          , j <- [t .. t + h - 1]]

part1 = length . filter repeated . group . sort . concatMap claimSquares
  where repeated xs = length xs > 1

part2 claims = ""

main = do
  handle <- openFile "03.txt" ReadMode
  contents <- hGetContents handle
  let claims = map parseClaim $ lines contents
  print $ part1 claims
  print $ part2 claims
  hClose handle

