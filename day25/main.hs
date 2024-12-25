enumerateInner :: Int -> [a] -> [(Int, a)]
enumerateInner _ [] = []
enumerateInner idx (first : remaining) = (idx, first) : enumerateInner (idx + 1) remaining

enumerate :: [a] -> [(Int, a)]
enumerate = enumerateInner 0

skipWhile :: (a -> Bool) -> [a] -> [a]
skipWhile _ [] = []
skipWhile shouldSkip (start : rest) =
  if shouldSkip start
    then skipWhile shouldSkip rest
    else start : rest

flatten :: [[a]] -> [a]
flatten = foldr1 (++)

split :: (a -> Bool) -> [a] -> [[a]]
split shouldSplit list =
  let processedList = skipWhile shouldSplit list
      firstGroup = takeWhile (not . shouldSplit) processedList
      remaining = skipWhile (not . shouldSplit) processedList
   in if null processedList then [] else firstGroup : split shouldSplit remaining

transpose :: [[a]] -> [[a]]
transpose [first] = map (: []) first
transpose (first : remaining) = zipWith (:) first $ transpose remaining

main = do
  contents <- getContents
  let schematics = split null $ lines contents
      locks = map (map ((+ (-1)) . length . takeWhile (== '#')) . transpose) $ filter (all (== '#') . head) schematics
      keys = map (map ((+ (-1)) . length . takeWhile (== '#') . reverse) . transpose) $ filter (all (== '#') . last) schematics
      combos = [(lock, key) | lock <- locks, key <- keys, all (<= 5) $ zipWith (+) lock key]
  print $ length combos
