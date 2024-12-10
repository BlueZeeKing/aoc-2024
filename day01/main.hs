import Data.Char (isSpace)

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort [x] = [x]
sort (pivot : remaining) = sort (filter (<= pivot) remaining) ++ [pivot] ++ sort (filter (> pivot) remaining)

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
parse input = map (map read . split isSpace) $ split (== '\n') input

transpose :: [[a]] -> [[a]]
transpose [first] = map (: []) first
transpose (first : remaining) = zipWith (:) first $ transpose remaining

main = do
  contents <- getContents
  print $
    let [first, second] = transpose $ parse contents
     in sum $ map (\value -> value * length (filter (== value) second)) first
