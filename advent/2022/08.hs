import System.IO (print, readFile, putStrLn)
import Data.Char (digitToInt)
import Data.List (transpose, intercalate)
import Data.Tuple (swap)
import qualified Data.Set as Set

-- heights of trees in a row from the point of view of a given tree
data TreeInRow = TreeInRow { toTheLeft  :: [Int] -- in reverse order
                           , treeHeight :: Int
                           , toTheRight :: [Int]
                           } deriving Show

isLastTree = null . toTheRight

nextTree (TreeInRow left me (next:right)) = TreeInRow (me:left) next right

treesInRow (t:ts) = takeUpTo isLastTree $ iterate nextTree $ TreeInRow [] t ts

-- takes while not matching, but includes the first non-matching value
takeUpTo p (x:xs) = if p x then [x] else x : (takeUpTo p xs)
takeUpTo p [] = []

isVisible (TreeInRow left me right) = all (<me) left || all (<me) right

visibleTreeIndices row =
  [ j | (j, t) <- zip [0..] (treesInRow row)
      , isVisible t ]

horizontallyVisibleTreeCoordinates grid =
  [ (i, j) | (i, row) <- zip [0..] grid
           , j <- visibleTreeIndices row ]

verticallyVisibleTreeCoordinates grid =
  map swap $ horizontallyVisibleTreeCoordinates $ transpose grid

visibleTreeCoordinates grid =
  let hv = Set.fromList $ horizontallyVisibleTreeCoordinates grid
      vv = Set.fromList $ verticallyVisibleTreeCoordinates grid
  in Set.union hv vv

directionalScenicScore height neighbors = length $ takeUpTo (>=height) neighbors

treeInRowScenicScores (TreeInRow l h r) = (f l, f r)
  where f = directionalScenicScore h

horizontalScenicScoresGrid grid = map (map treeInRowScenicScores . treesInRow) grid

verticalScenicScoresGrid grid = transpose $ horizontalScenicScoresGrid $ transpose grid

scenicScoreGrid grid =
  let hsc = horizontalScenicScoresGrid grid
      vsc = verticalScenicScoresGrid grid
  in elemwise multScores hsc vsc
  where
    elemwise = zipWith . zipWith
    multScores (a, b) (c, d) = a * b * c * d

part1 = Set.size . visibleTreeCoordinates

part2 = maximum . map maximum . scenicScoreGrid

example = [ [3, 0, 3, 7, 3]
          , [2, 5, 5, 1, 2]
          , [6, 5, 3, 3, 2]
          , [3, 3, 5, 4, 9]
          , [3, 5, 3, 9, 0] ] :: [[Int]]

main = do
  contents <- readFile "08.txt"
  let grid = filter (not . null) $ map (map digitToInt) $ lines contents
  print $ part1 grid
  print $ part2 grid

