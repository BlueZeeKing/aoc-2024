import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

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

listToMapOfLists :: (Ord a) => [(a, b)] -> Map.Map a [b]
listToMapOfLists = foldr (\(key, value) acc -> if key `Map.member` acc then Map.adjust (value :) key acc else Map.insert key [value] acc) Map.empty

findPossibleLoops :: Map.Map String [String] -> String -> Int -> [[String]]
findPossibleLoops connectionsMap target 0 = map (\connection -> [target, connection]) $ connectionsMap Map.! target
findPossibleLoops connectionsMap target depth =
  map (target :) $ flatten $ map (\nextConnection -> findPossibleLoops connectionsMap nextConnection (depth - 1)) $ connectionsMap Map.! target

findLoops :: Map.Map String [String] -> String -> Set.Set [String]
findLoops connectionsMap target = Set.fromList $ map (sort . take 3) $ filter (\val -> last val == head val) $ findPossibleLoops connectionsMap target 2

main = do
  contents <- getContents
  let connections = map ((\[a, b] -> (a, b)) . split (== '-')) $ lines contents
      connectionsMap = listToMapOfLists (connections ++ map (\(a, b) -> (b, a)) connections)
      ts = filter ((== 't') . head) $ map fst $ Map.toList connectionsMap
      loops = Set.unions $ map (findLoops connectionsMap) ts
  print $ length loops
