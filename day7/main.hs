skipWhile :: (a -> Bool) -> [a] -> [a]
skipWhile _ [] = []
skipWhile predicate (start : rest) = if predicate start then skipWhile predicate rest else start : rest

split :: (a -> Bool) -> [a] -> [[a]]
split predicate list =
  let processed_list = skipWhile predicate list
      first_group = takeWhile (not . predicate) processed_list
      remaining = skipWhile (not . predicate) processed_list
   in if null processed_list then [] else first_group : split predicate remaining

canBeCalculated :: Int -> [Int] -> Bool
canBeCalculated 0 _ = True
canBeCalculated _ [] = False
canBeCalculated total (first : remaining)
  | total < 0 = False
  | otherwise = canBeCalculated (total - first) remaining || (total `mod` first == 0 && canBeCalculated (total `div` first) remaining)

main = do
  contents <- getContents
  let parsedInput :: [(Int, [Int])] = map ((\[total, values] -> (read total, reverse $ map read $ split (== ' ') values)) . split (== ':')) $ split (== '\n') contents
   in print $ sum $ map fst $ filter (uncurry canBeCalculated) parsedInput
