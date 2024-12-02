import Data.List
import Data.Ord

day2part1 :: [[Int]] -> Int
day2part1 input =
  length $ filter isSafe input

day2part2 :: [[Int]] -> Int
day2part2 input =
  length $ filter (\report -> isSafe report || isSafeByRemovingAny report) input

isSafe :: [Int] -> Bool
isSafe report =
  isAscOrDesc report && distancesOk report

isAscOrDesc :: [Int] -> Bool
isAscOrDesc report =
  sort report == report || sortBy (comparing Data.Ord.Down) report == report

distancesOk :: [Int] -> Bool
distancesOk report =
  case report of
    [] -> True
    [x] -> True
    [x, y] -> abs (y - x) >= 1 && abs (y - x) <= 3
    x : y : xs -> distancesOk [x, y] && distancesOk (y : xs)

getShorterLists :: [Int] -> [[Int]]
getShorterLists report =
  map (\i -> (fst (splitAt i report)) ++ (tail $ snd (splitAt i report))) [0 .. (length report) - 1]

isSafeByRemovingAny :: [Int] -> Bool
isSafeByRemovingAny report =
  any isSafe $ getShorterLists report

readInt :: String -> Int
readInt = read

parse :: String -> [[Int]]
parse input =
  map (map readInt . words) (lines input)

sample :: [[Int]]
sample =
  [ [7, 6, 4, 2, 1],
    [1, 2, 7, 8, 9],
    [9, 7, 6, 2, 1],
    [1, 3, 2, 4, 5],
    [8, 6, 4, 4, 1],
    [1, 3, 6, 7, 9]
  ]

main :: IO ()
main = do
  contents <- readFile "day2.txt"
  print $ day2part1 $ parse contents
  print $ day2part2 $ parse contents