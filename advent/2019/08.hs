import System.IO (readFile)
import Data.Char (digitToInt, isDigit)
import Data.List (splitAt, minimumBy)
import Data.Ord  (comparing)

data Counter = Counter { c0 :: Int
                       , c1 :: Int
                       , c2 :: Int
                       }

updateCounter :: Counter -> Int -> Counter
updateCounter (Counter c0 c1 c2) 0 = Counter (c0 + 1) c1      c2
updateCounter (Counter c0 c1 c2) 1 = Counter  c0     (c1 + 1) c2
updateCounter (Counter c0 c1 c2) 2 = Counter  c0      c1     (c2 + 1)
updateCounter c _ = c

count :: Foldable f => f Int -> Counter
count = foldl updateCounter $ Counter 0 0 0

groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs = reverse $ g xs []
  where
    g [] gs = gs
    g xs gs =
      let (group, rest) = splitAt n xs
      in  g rest (group : gs)

part1 layers =
  let Counter _ ones twos = minimumBy (comparing c0) $ map count layers
  in  ones * twos

part2 layers =
  let rows   = groupsOf 25 $ foldl1 (zipWith combinePixels) layers
  in  unlines $ map (concatMap showPixel) rows
  where
    combinePixels 2 b = b
    combinePixels f b = f
    showPixel 0 = " "
    showPixel 1 = "█"
    showPixel 2 = "."

main = do
  contents <- readFile "08.txt"
  let sif = map digitToInt $ filter isDigit contents
  let layers = groupsOf (25 * 6) sif
  print    $ part1 layers
  putStrLn $ part2 layers
