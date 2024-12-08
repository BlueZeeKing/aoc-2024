import Data.Maybe (fromMaybe)

skipWhile :: (a -> Bool) -> [a] -> [a]
skipWhile _ [] = []
skipWhile predicate (start : rest) = if predicate start then skipWhile predicate rest else start : rest

split :: (a -> Bool) -> [a] -> [[a]]
split predicate list =
  let processed_list = skipWhile predicate list
      first_group = takeWhile (not . predicate) processed_list
      remaining = skipWhile (not . predicate) processed_list
   in if null processed_list then [] else first_group : split predicate remaining

undoConcat :: Int -> Int -> Maybe Int
undoConcat remaining 0 = Just remaining
undoConcat 0 _ = Nothing
undoConcat target pattern =
  let splitNumber number = (number `div` 10, number `mod` 10)
      (targetRemaining, targetDigit) = splitNumber target
      (patternRemaining, patternDigit) = splitNumber pattern
   in if targetDigit == patternDigit then undoConcat targetRemaining patternRemaining else Nothing

canBeCalculated :: Int -> [Int] -> Bool
canBeCalculated 0 [] = True
canBeCalculated _ [] = False
canBeCalculated 0 _ = False
canBeCalculated total (first : remaining)
  | total < 0 = False
  | otherwise =
      canBeCalculated (total - first) remaining
        || (total `mod` first == 0 && canBeCalculated (total `div` first) remaining)
        || maybe False (`canBeCalculated` remaining) (undoConcat total first)

main = do
  contents <- getContents
  let parsedInput :: [(Int, [Int])] = map ((\[total, values] -> (read total, reverse $ map read $ split (== ' ') values)) . split (== ':')) $ split (== '\n') contents
   in print $ sum $ map fst $ filter (uncurry canBeCalculated) parsedInput
