import Data.List (reverse, sort, splitAt)

parse :: String -> [[Int]]
parse input =
  map (map readInt) (map words $ lines input)

-- print . map readInt . words $ contents

-- alternately, main = print . map readInt . words =<< readFile "test.txt"

readInt :: String -> Int
readInt = read

sample :: [[Int]]
sample =
  [ [7, 6, 4, 2, 1],
    [1, 2, 7, 8, 9],
    [9, 7, 6, 2, 1],
    [1, 3, 2, 4, 5],
    [8, 6, 4, 4, 1],
    [1, 3, 6, 7, 9]
  ]

pairs [] = []
pairs [x] = []
pairs (x : y : rest) = (x, y) : pairs (y : rest)

isAscOrDesc report =
  ascSortedReported == report || descSortedReport == report
  where
    ascSortedReported = sort report
    descSortedReport = reverse $ sort $ report

distancesOk :: [Int] -> Bool
distancesOk report =
  case report of
    [] -> True
    [x] -> True
    [x, y] -> abs (y - x) >= 1 && abs (y - x) <= 3
    x : xs -> abs (x - (head xs)) >= 1 && abs (x - head xs) <= 3 && distancesOk xs

isSafe report =
  isAscOrDesc report && distancesOk report

getShorterLists :: [Int] -> [[Int]]
getShorterLists report =
  map (\i -> (fst (splitAt i report)) ++ (tail $ snd (splitAt i report))) [0 .. (length report) - 1]

-- map (\i -> report !! i) [0 .. (length report) - 1]

isSafeByRemovingAny :: [Int] -> Bool
isSafeByRemovingAny report =
  any isSafe $ getShorterLists report

isSafeWithDampener :: [Int] -> Bool
isSafeWithDampener report =
  isSafe report || (isSafeByRemovingAny report)

day2part1 input =
  length $ filter isSafe input

day2part2 :: [[Int]] -> Int
day2part2 input =
  length $ filter isSafeWithDampener input

main :: IO ()
main = do
  contents <- readFile "day2.txt"
  print $ day2part1 $ parse contents
  print $ day2part2 $ parse contents