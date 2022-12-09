import System.IO (print, readFile)
import qualified Data.Set as Set

moveKnot 'U' (x, y) = (x, y + 1)
moveKnot 'D' (x, y) = (x, y - 1)
moveKnot 'R' (x, y) = (x + 1, y)
moveKnot 'L' (x, y) = (x - 1, y)

follow (hx, hy) (tx, ty)
  -- already touching
  | abs (hx - tx) <= 1 && abs (hy - ty) <= 1 = (tx, ty)
  -- two straight steps away
  | hx == tx && hy == ty - 2 = (tx, ty - 1)
  | hx == tx && hy == ty + 2 = (tx, ty + 1)
  | hy == ty && hx == tx - 2 = (tx - 1, ty)
  | hy == ty && hx == tx + 2 = (tx + 1, ty)
  -- need to move diagonally
  | hx < tx && hy < ty = (tx - 1, ty - 1)
  | hx < tx && hy > ty = (tx - 1, ty + 1)
  | hx > tx && hy < ty = (tx + 1, ty - 1)
  | hx > tx && hy > ty = (tx + 1, ty + 1)
  -- should not happen
  | otherwise = error "head is too far away"

makeAllKnotsFollowHead (h:k:ks) =
  let hNew = follow h k
  in hNew : (makeAllKnotsFollowHead (hNew:ks))
makeAllKnotsFollowHead _ = []

nextRope (h:ks) dir =
  let hNew = moveKnot dir h
  in hNew : (makeAllKnotsFollowHead (hNew:ks))

lineToDirections [[d], n] = take (read n) $ repeat d
lineToDirections [] = []

solve rope dirs =
  let allRopes = scanl nextRope rope dirs
  in Set.size $ Set.fromList $ map last allRopes

initialRope ropeLength = take ropeLength $ repeat (0, 0)

part1 = solve $ initialRope 2
part2 = solve $ initialRope 10

main = do
  contents <- readFile "09.txt"
  let dirs = concatMap (lineToDirections . words) $ lines contents

  print $ part1 dirs
  print $ part2 dirs

