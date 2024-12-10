import qualified Data.Set as Set
import Distribution.Utils.Generic

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

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

findEndPoints :: Int -> Int -> [[Int]] -> [(Int, Int)]
findEndPoints row col input =
  let currentValue = input !! row !! col
      width = length $ head input
      height = length input
      spotsToCheck = filter (\(row, col) -> input !! row !! col == currentValue + 1) $ filter (\(row, col) -> row >= 0 && col >= 0 && row < height && col < width) [(row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)]
   in if currentValue == 8 then spotsToCheck else flatten $ filter (not . null) $ map (\(row, col) -> findEndPoints row col input) spotsToCheck

findScore :: Int -> Int -> [[Int]] -> Int
findScore row col input =
  let endpoints = findEndPoints row col input
   in length endpoints

main = do
  contents <- getContents
  let input = map (map charToInt) $ split (== '\n') contents
      inputWithIdx =
        flatten $
          map (\(row_idx, row) -> map (\(col_idx, value) -> (row_idx, col_idx, value)) row) $
            enumerate $
              map enumerate input
      trailHeads = filter ((== 0) . trdOf3) inputWithIdx
   in print $ sum $ map (\(row, col, _) -> findScore row col input) trailHeads
