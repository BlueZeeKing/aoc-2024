skipWhile :: (a -> Bool) -> [a] -> [a]
skipWhile _ [] = []
skipWhile predicate (start : rest) = if predicate start then skipWhile predicate rest else start : rest

split :: (a -> Bool) -> [a] -> [[a]]
split predicate list =
  let processed_list = skipWhile predicate list
      first_group = takeWhile (not . predicate) processed_list
      remaining = skipWhile (not . predicate) processed_list
   in if null processed_list then [] else first_group : split predicate remaining

parse :: String -> [[Int]]
parse input = map (map read . split (== ' ')) $ split (== '\n') input

getDiffs :: [Int] -> [Int]
getDiffs [_] = []
getDiffs (first : second : rest) = second - first : getDiffs (second : rest)

countSigns :: [Int] -> (Int, Int)
countSigns input =
  let sumPositive = length $ filter (> 0) input
   in (sumPositive, length input - sumPositive)

isValid :: [Int] -> Bool
isValid list =
  let diffs = getDiffs list
      (numPos, numNeg) = countSigns diffs
   in (numPos == 0 || numNeg == 0) && all ((\val -> val >= 1 && val <= 3) . abs) diffs

main = do
  contents <- getContents
  print $ length $ filter isValid $ parse contents
