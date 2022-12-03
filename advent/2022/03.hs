import System.IO
import qualified Data.Map as Map
import qualified Data.Set as Set

halves xs =
  let half = div (length xs) 2
  in (take half xs, drop half xs)

priorityMap =
  let letters = ['a' .. 'z'] ++ ['A' .. 'Z']
  in Map.fromList $ zip letters [1..]

priority item = priorityMap Map.! item

commonItem xs ys =
  let xSet = Set.fromList xs
      ySet = Set.fromList ys
      commonSet = Set.intersection xSet ySet
  in Set.findMin commonSet

commonItem3 xs ys zs =
  let xSet = Set.fromList xs
      ySet = Set.fromList ys
      zSet = Set.fromList zs
      commonSet = Set.intersection xSet $ Set.intersection ySet zSet
  in Set.findMin commonSet

uncurry3 f (x, y, z) = f x y z

groupsOfThree (a:b:c:xs) = (a, b, c) : (groupsOfThree xs)
groupsOfThree lessThanThree = []

part1 = sum . map (priority . uncurry commonItem . halves)

part2 = sum . map (priority . uncurry3 commonItem3) . groupsOfThree

main = do
  content <- readFile "03.txt"
  let input = lines content

  print $ part1 input
  print $ part2 input
