import qualified Data.Set as Set
import System.IO

firstRepeated xs = fr Set.empty xs
  where
    fr seen (x:xs)
      | Set.member x seen = x
      | otherwise         = fr (Set.insert x seen) xs

toInt ('+' : rst) = read rst :: Int
toInt s           = read s   :: Int

part1 = sum

part2 = firstRepeated . (scanl1 (+)) . cycle

main = do
  handle <- openFile "01.txt" ReadMode
  contents <- hGetContents handle
  let freqs = map toInt (lines contents)
  print $ part1 freqs
  print $ part2 freqs
  hClose handle

