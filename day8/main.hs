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
flatten [] = []
flatten [[]] = []
flatten ([] : (next : innerRemaining) : remaining) = next : flatten (innerRemaining : remaining)
flatten ((next : innerRemaining) : remaining) = next : flatten (innerRemaining : remaining)

trd :: (a, b, c) -> c
trd (_, _, val) = val

validPosition :: Int -> Int -> Int -> Int -> Bool
validPosition width height row col = row >= 0 && col >= 0 && row < height && col < width

findAntinodesForOne :: Int -> Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
findAntinodesForOne _ _ _ _ [] = []
findAntinodesForOne width height row col ((towerRow, towerCol) : remaining)
  | row == towerRow && col == towerCol = nextResult
  | otherwise = afterNodes ++ beforeNodes ++ nextResult
  where
    nextResult = findAntinodesForOne width height row col remaining
    applyMultiplier multiplier = (row + ((row - towerRow) * multiplier), col + ((col - towerCol) * multiplier))
    afterNodesMultipliers = [1, 2 ..]
    beforeNodesMultipliers = [0, -1 ..]
    afterNodes = takeWhile (uncurry $ validPosition width height) $ map applyMultiplier afterNodesMultipliers
    beforeNodes = takeWhile (uncurry $ validPosition width height) $ map applyMultiplier beforeNodesMultipliers

findAntinodes :: Int -> Int -> [(Int, Int)] -> Set.Set (Int, Int)
findAntinodes width height towers = foldr Set.insert Set.empty $ flatten $ filter (not . null) $ map (\(row, col) -> findAntinodesForOne width height row col towers) towers

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
      antinodes = foldr1 Set.union $ map (findAntinodes width height . snd) $ Map.toList frequencies
   in print $ length antinodes
