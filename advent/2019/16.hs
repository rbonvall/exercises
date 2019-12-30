import System.IO
import Data.Char (isDigit, digitToInt)


fftMatrix pattern = map nthRow [1..]
  where nthRow n = tail $ cycle $ concatMap (replicate n) pattern

m :: [[Int]]
m = fftMatrix [0, 1, 0, -1]

fft :: [[Int]] -> Int -> [Int] -> [Int]
fft matrix signalLength signal =
  let rows = take signalLength matrix
  in  map processRow rows
  where processRow row = (abs $ sum $ zipWith (*) signal row) `mod` 10

numberFromDigits ds = foldl f 0 ds
  where f n d = 10 * n + d

nApply n f x = napp n f x x
  where
    napp 0 f x y = y
    napp n f x y = napp (n - 1) f x (f y)


part1 signal =
  let n = length signal
      output = nApply 100 (fft m n) signal
  in numberFromDigits $ take 8 output

part2 signal =
  let newSignal = concat (replicate 10000 signal)
      n = 10000 * length signal
      offset = numberFromDigits $ take 7 newSignal
      output = nApply 100 (fft m n) newSignal
  in numberFromDigits $ take 8 $ drop offset output
  -- stack overflow :(

main = do
  contents <- readFile "16.txt"
  let signal = map digitToInt $ filter isDigit contents
  print $ part1 signal
  print $ part2 signal
