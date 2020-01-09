import System.IO

data Reaction = Reaction [(Int, String)] (Int, String)   deriving (Show, Eq)

parseReaction :: String -> Reaction
parseReaction line =
  let quantifiedChemicals = pairs $ words $ filter (not . isSymbol) line
  in Reaction (init quantifiedChemicals) (last quantifiedChemicals)
  where
    isSymbol x = elem x ",=>"
    pairs (x0:x1:xs) = (read x0 :: Int, x1) : pairs xs
    pairs _ = []

part1 x = x

main = do
  contents <- readFile "14.txt"
  let reactions = map parseReaction $ lines contents
  print $ part1 reactions
