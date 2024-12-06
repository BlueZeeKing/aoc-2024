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

enumerate :: [a] -> [(Int, a)]
enumerate [] = []
enumerate (first : remaining) = (0, first) : map (\(idx, val) -> (idx + 1, val)) (enumerate remaining)

flatten :: [[a]] -> [a]
flatten [[]] = []
flatten ([] : (next : innerRemaining) : remaining) = next : flatten (innerRemaining : remaining)
flatten ((next : innerRemaining) : remaining) = next : flatten (innerRemaining : remaining)

skip :: Int -> [a] -> [a]
skip _ [] = []
skip 0 rest = rest
skip amount_to_skip (_ : rest) = skip (amount_to_skip - 1) rest

setVisited :: [[Char]] -> Int -> Int -> [[Char]]
setVisited input row col =
  let aboveRows = take row input
      belowRows = skip (row + 1) input
      currentRow = input !! row
      beforeCols = take col currentRow
      afterCols = skip (col + 1) currentRow
   in aboveRows ++ [beforeCols ++ ['V'] ++ afterCols] ++ belowRows

moveUp :: [[Char]] -> Int -> Int -> Int
moveUp input row col
  | row == 0 = if input !! row !! col == 'V' then 0 else 1
  | input !! (row - 1) !! col == '#' = moveRight input row col
  | otherwise = (if input !! row !! col == 'V' then 0 else 1) + moveUp (setVisited input row col) (row - 1) col

moveDown :: [[Char]] -> Int -> Int -> Int
moveDown input row col
  | row == length input - 1 = if input !! row !! col == 'V' then 0 else 1
  | input !! (row + 1) !! col == '#' = moveLeft input row col
  | otherwise = (if input !! row !! col == 'V' then 0 else 1) + moveDown (setVisited input row col) (row + 1) col

moveLeft :: [[Char]] -> Int -> Int -> Int
moveLeft input row col
  | col == 0 = if input !! row !! col == 'V' then 0 else 1
  | input !! row !! (col - 1) == '#' = moveUp input row col
  | otherwise = (if input !! row !! col == 'V' then 0 else 1) + moveLeft (setVisited input row col) row (col - 1)

moveRight :: [[Char]] -> Int -> Int -> Int
moveRight input row col
  | col == length (head input) - 1 = if input !! row !! col == 'V' then 0 else 1
  | input !! row !! (col + 1) == '#' = moveDown input row col
  | otherwise = (if input !! row !! col == 'V' then 0 else 1) + moveRight (setVisited input row col) row (col + 1)

main = do
  contents <- getContents
  let input = split (== '\n') contents
      (startingRow, startingCol, _) =
        head $
          filter (\(_, _, val) -> val == '^') $
            flatten $
              map (\(row_idx, row) -> map (\(col_idx, value) -> (row_idx, col_idx, value)) row) $
                enumerate $
                  map enumerate input
   in print $ moveUp input startingRow startingCol
