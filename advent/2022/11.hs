import System.IO (print, readFile, putStrLn)
import Data.List (transpose, sort)

groups [] = []
groups ("" : ss) = groups ss
groups ls =
  let (firstGroup, rest) = break null ls
  in firstGroup : groups rest

data Monkey = Monkey { operation :: Integer -> Integer
                     , test      :: Integer -> Bool
                     , whenTrue  :: Integer
                     , whenFalse :: Integer
                     } deriving Show

instance Num n => Show (n -> a) where show f = "<func>"

data Sim = Sim { monkeys :: [Monkey]
               , worryFunction :: Integer -> Integer
               }

parseMonkeyLines :: [String] -> (Monkey, [Integer])
parseMonkeyLines [_, s, o, d, t, f] =
  let m = Monkey { operation = parseOp     o
                 , test      = parseTest   d
                 , whenTrue  = parseAction t
                 , whenFalse = parseAction f
                 }
      items = parseItems s
  in (m, items)
  where
    parseItems s = map (read . filter (/= ',')) $ drop 2 $ words s
    parseOp o = compileFormula $ drop 3 $ words o
    parseTest d = divisibleBy $ read $ last $ words d
    parseAction a = read $ last $ words a

compileFormula ["old", "*", "old"] = (^2)
compileFormula ["old", "*", n    ] = (* (read n :: Integer))
compileFormula ["old", "+", n    ] = (+ (read n :: Integer))

divisibleBy d n = n `mod` d == 0

data KeepAwayState = KeepAwayState [[Integer]] deriving Show

toBeThrownBy f (Monkey op test whenTrue whenFalse) item =
  let w = f $ op item
      monkeyToThrowTo = if test w then whenTrue else whenFalse
  in (w, monkeyToThrowTo)

-- make the ith list empty
emptyList i (l:ls) =
  if i == 0 then []:ls else l:(emptyList (i - 1) ls)

throwAll toThrow itemsPerMonkey =
  [ items ++ newItems
  | (i, items) <- zip [0..] itemsPerMonkey
  , let newItems = [ what | (what, to) <- toThrow, to == i ]
  ]

keepAwayTurn worryFunction (KeepAwayState itemsPerMonkey) (i, monkey) =
  let toThrow = map (toBeThrownBy worryFunction monkey) (itemsPerMonkey !! i)
      itemsAfterThrowing = emptyList i $ throwAll toThrow itemsPerMonkey
  in KeepAwayState itemsAfterThrowing

-- compute states after each turn in a round
keepAwayRoundStates :: Sim -> KeepAwayState -> [KeepAwayState]
keepAwayRoundStates (Sim monkeys w) state =
  let indexedMonkeys = zip [0..] monkeys
  in scanl (keepAwayTurn w) state indexedMonkeys

allRoundStates :: Sim -> KeepAwayState -> [[KeepAwayState]]
allRoundStates sim state0 =
  drop 1 $ iterate (keepAwayRoundStates sim . last) [state0]

countRoundInspectionsByMonkey :: [KeepAwayState] -> [Int]
countRoundInspectionsByMonkey roundStates =
  let roundStatesWithIndex = take (length roundStates - 1) $ zip roundStates [0..]
  in  map monkeyItemsBeforeInspecting roundStatesWithIndex
    where monkeyItemsBeforeInspecting (KeepAwayState s, i) = length (s !! i)

addAll ls = foldl (zipWith (+)) (map (const 0) ls) ls

computeMonkeyBusiness rounds =
  let counts = map countRoundInspectionsByMonkey rounds
      totalCountsPerMonkey = addAll counts
      mostActiveMonkeys = take 2 $ reverse $ sort totalCountsPerMonkey
  in product $ map toInteger $ mostActiveMonkeys

solve simulationRules nrRounds initialState =
  let allRounds = allRoundStates simulationRules initialState
      rounds = take nrRounds allRounds
  in computeMonkeyBusiness rounds


part1 monkeys s0 = solve (Sim monkeys (`div` 3)) 20 s0
part2 monkeys s0 = solve (Sim monkeys id) 20 s0   -- 10000 s0

example = [ (Monkey (*19) (divisibleBy 23) 2 3, [79, 98] )
          , (Monkey (+ 6) (divisibleBy 19) 2 0, [54, 65, 75, 74])
          , (Monkey (^ 2) (divisibleBy 13) 1 3, [79, 60, 97])
          , (Monkey (+ 3) (divisibleBy 17) 0 1, [74])
          ] :: [(Monkey, [Integer])]

(exampleMonkeys, exampleItems) = unzip example
exampleRounds1 = allRoundStates (Sim exampleMonkeys (`div` 3)) $ KeepAwayState exampleItems
exampleRounds2 = allRoundStates (Sim exampleMonkeys id) $ KeepAwayState exampleItems

main = do
  contents <- readFile "11.txt"
  let (monkeys, items) = unzip $ map parseMonkeyLines $ groups $ lines contents
  let state0 = KeepAwayState items

  putStrLn $ "Example (" ++ (show (length exampleMonkeys)) ++ " monkeys)"
  print $ part1 exampleMonkeys $ KeepAwayState exampleItems
  print $ part2 exampleMonkeys $ KeepAwayState exampleItems

  putStrLn $ "Problem (" ++ (show (length monkeys)) ++ " monkeys)"
  print $ part1 monkeys state0
  print $ part2 monkeys state0



