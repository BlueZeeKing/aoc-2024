import qualified Data.Set as Set

width = 71

height = 71

skipWhile :: (a -> Bool) -> [a] -> [a]
skipWhile _ [] = []
skipWhile predicate (start : rest) =
  if predicate start
    then skipWhile predicate rest
    else start : rest

split :: (a -> Bool) -> [a] -> [[a]]
split predicate list =
  let processedList = skipWhile predicate list
      firstGroup = takeWhile (not . predicate) processedList
      remaining = skipWhile (not . predicate) processedList
   in if null processedList then [] else firstGroup : split predicate remaining

bfs :: Set.Set (Int, Int) -> [(Int, Int, Int)] -> Int
bfs barriersOrVisited ((x, y, cost) : remaining)
  | x == width - 1 && y == height - 1 = cost
  | otherwise =
      let surrounding = filter (\(x, y) -> x >= 0 && y >= 0 && x < width && y < height && Set.notMember (x, y) barriersOrVisited) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
       in bfs (foldr Set.insert barriersOrVisited surrounding) (remaining ++ map (\(x, y) -> (x, y, cost + 1)) surrounding)

main = do
  contents <- getContents
  let barriers :: [(Int, Int)] = map ((\[x, y] -> (x, y)) . map read . split (== ',')) $ lines contents
  print $ bfs (Set.fromList $ take 1024 barriers) [(0, 0, 0)]
