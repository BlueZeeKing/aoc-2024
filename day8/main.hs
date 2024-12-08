import qualified Data.Map as Map
import qualified Data.Set as Set

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

enumerate :: [a] -> [(Int, a)]
enumerate [] = []
enumerate (first : remaining) = (0, first) : map (\(idx, val) -> (idx + 1, val)) (enumerate remaining)

flatten :: [[a]] -> [a]
flatten [[]] = []
flatten ([] : (next : innerRemaining) : remaining) = next : flatten (innerRemaining : remaining)
flatten ((next : innerRemaining) : remaining) = next : flatten (innerRemaining : remaining)

trd :: (a, b, c) -> c
trd (_, _, val) = val

findAntinodesForOne :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
findAntinodesForOne _ _ [] = []
findAntinodesForOne row col ((towerRow, towerCol) : remaining)
  | row == towerRow && col == towerCol = findAntinodesForOne row col remaining
  | otherwise = (row + row - towerRow, col + col - towerCol) : findAntinodesForOne row col remaining

findAntinodes :: [(Int, Int)] -> Set.Set (Int, Int)
findAntinodes towers = foldr Set.insert Set.empty $ flatten $ map (\(row, col) -> findAntinodesForOne row col towers) towers

main = do
  contents <- getContents
  let input = split (== '\n') contents
      inputWithIdx =
        filter ((/= '.') . trd) $
          flatten $
            map (\(row_idx, row) -> map (\(col_idx, value) -> (row_idx, col_idx, value)) row) $
              enumerate $
                map enumerate input
      height = length input
      width = length $ head input
      frequencies = foldr (\(row_idx, col_idx, val) acc -> if Map.member val acc then Map.adjust ((row_idx, col_idx) :) val acc else Map.insert val [(row_idx, col_idx)] acc) Map.empty inputWithIdx
      antinodes = foldr1 Set.union $ map (findAntinodes . snd) $ Map.toList frequencies
      filteredAntinodes = filter (\(row, col) -> row >= 0 && col >= 0 && row < height && col < width) $ Set.toList antinodes
   in print $ length filteredAntinodes
