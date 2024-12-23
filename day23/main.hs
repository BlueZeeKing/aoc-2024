import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace

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

listToMapOfSets :: (Ord a, Ord b) => [(a, b)] -> Map.Map a (Set.Set b)
listToMapOfSets = foldr (\(key, value) acc -> if key `Map.member` acc then Map.adjust (Set.insert value) key acc else Map.insert key (Set.fromList [value]) acc) Map.empty

findPossibleLoops :: Map.Map String (Set.Set String) -> String -> Int -> [[String]]
findPossibleLoops connectionsMap target 0 = map (\connection -> [target, connection]) $ Set.toList $ connectionsMap Map.! target
findPossibleLoops connectionsMap target depth =
  map (target :) $ flatten $ map (\nextConnection -> findPossibleLoops connectionsMap nextConnection (depth - 1)) $ Set.toList $ connectionsMap Map.! target

findLoops :: Map.Map String (Set.Set String) -> String -> Set.Set [String]
findLoops connectionsMap target = Set.fromList $ map (sort . take 3) $ filter (\val -> last val == head val) $ findPossibleLoops connectionsMap target 2

findBiggestConnection :: Map.Map String (Set.Set String) -> String -> Set.Set String -> [String]
findBiggestConnection connectionsMap current visited
  | current `Map.notMember` connectionsMap = []
  | otherwise =
      let possibilities =
            map (\connection -> findBiggestConnection connectionsMap connection (Set.insert connection visited)) $
              filter (\connection -> connection `Map.member` connectionsMap && connection `Set.notMember` visited && visited `Set.isSubsetOf` (connectionsMap Map.! connection)) $
                Set.toList $
                  connectionsMap Map.! current
       in if null possibilities
            then [current]
            else current : maximumBy (\a b -> length a `compare` length b) possibilities

mapWithState :: (b -> a -> (c, b)) -> b -> [a] -> [c]
mapWithState _ _ [] = []
mapWithState function state (current : remaining) =
  let (mappedCurrent, newState) = function state current
   in mappedCurrent : mapWithState function newState remaining

main = do
  contents <- getContents
  let connections = map ((\[a, b] -> (a, b)) . split (== '-')) $ lines contents
      connectionsMap = listToMapOfSets (connections ++ map (\(a, b) -> (b, a)) connections)
      computers = Set.toList $ Set.fromList $ map fst connections
      loops = Set.unions $ map (findLoops connectionsMap) computers
      groups =
        Set.toList
          $ Set.fromList
          $ mapWithState
            ( \state loop ->
                (sort $ tail loop ++ findBiggestConnection state (head loop) (Set.fromList loop), foldr Map.delete state loop)
            )
            connectionsMap
          $ Set.toList loops
      largestGroup = maximumBy (\a b -> length a `compare` length b) groups
  putStrLn $ foldr1 (\val acc -> val ++ "," ++ acc) largestGroup
