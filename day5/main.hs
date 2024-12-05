skipWhile :: (a -> Bool) -> [a] -> [a]
skipWhile _ [] = []
skipWhile shouldSkip (start : rest) =
  if shouldSkip start
    then skipWhile shouldSkip rest
    else start : rest

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split shouldSplit list =
  let processedList = if shouldSplit $ head list then tail list else list
      firstGroup = takeWhile (not . shouldSplit) processedList
      remaining = skipWhile (not . shouldSplit) processedList
   in if null processedList then [] else firstGroup : split shouldSplit remaining

toTuple :: [a] -> (a, a)
toTuple [first, second] = (first, second)

isValidUpdate :: [(Int, Int)] -> [Int] -> Bool
isValidUpdate _ [] = True
isValidUpdate rules (first : remaining) =
  let allRequiredPreviousPages = map fst $ filter ((== first) . snd) rules
   in not (any (`elem` allRequiredPreviousPages) remaining) && isValidUpdate rules remaining

main = do
  contents <- getContents
  let [rawRules, rawUpdates] = split null $ split (== '\n') contents
      rules :: [(Int, Int)] = map (toTuple . map read . split (== '|')) rawRules
      updates :: [[Int]] = map (map read . split (== ',')) rawUpdates
      validUpdates = filter (isValidUpdate rules) updates
      middleValues = map (\update -> update !! (length update `div` 2)) validUpdates
   in print $ sum middleValues
