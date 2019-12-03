import System.IO
import Data.Sequence

data State = State Int (Seq Int)

part1 initialState = 0

main = do
  handle <- openFile "02.txt" ReadMode
  contents <- hGetContents handle
  let opList = read ("[" ++ contents ++ "]") :: [Int]
  print $ part1 $ State 0 (fromList opList)
  hClose handle
