import Data.List (sortBy)

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

data Direction = Up | Down | Left | Right
  deriving (Eq, Show)

compareRows :: (Int, Int) -> (Int, Int) -> Ordering
compareRows (fst, _) (snd, _) = fst `compare` snd

compareCols :: (Int, Int) -> (Int, Int) -> Ordering
compareCols (_, fst) (_, snd) = fst `compare` snd

setVisited :: [[Char]] -> Int -> Int -> [[Char]]
setVisited input row col =
  let aboveRows = take row input
      belowRows = skip (row + 1) input
      currentRow = input !! row
      beforeCols = take col currentRow
      afterCols = skip (col + 1) currentRow
   in aboveRows ++ [beforeCols ++ ['V'] ++ afterCols] ++ belowRows

moveUp :: [[Char]] -> Int -> Int -> [(Int, Int)]
moveUp input row col
  | row == 0 = if input !! row !! col == 'V' then [] else [(row, col)]
  | input !! (row - 1) !! col == '#' = moveRight input row col
  | otherwise = (if input !! row !! col == 'V' then [] else [(row, col)]) ++ moveUp (setVisited input row col) (row - 1) col

moveDown :: [[Char]] -> Int -> Int -> [(Int, Int)]
moveDown input row col
  | row == length input - 1 = if input !! row !! col == 'V' then [] else [(row, col)]
  | input !! (row + 1) !! col == '#' = moveLeft input row col
  | otherwise = (if input !! row !! col == 'V' then [] else [(row, col)]) ++ moveDown (setVisited input row col) (row + 1) col

moveLeft :: [[Char]] -> Int -> Int -> [(Int, Int)]
moveLeft input row col
  | col == 0 = if input !! row !! col == 'V' then [] else [(row, col)]
  | input !! row !! (col - 1) == '#' = moveUp input row col
  | otherwise = (if input !! row !! col == 'V' then [] else [(row, col)]) ++ moveLeft (setVisited input row col) row (col - 1)

moveRight :: [[Char]] -> Int -> Int -> [(Int, Int)]
moveRight input row col
  | col == length (head input) - 1 = if input !! row !! col == 'V' then [] else [(row, col)]
  | input !! row !! (col + 1) == '#' = moveDown input row col
  | otherwise = (if input !! row !! col == 'V' then [] else [(row, col)]) ++ moveRight (setVisited input row col) row (col + 1)

doesLoop :: [(Int, Int)] -> Int -> Int -> Direction -> [(Int, Int)] -> Bool
doesLoop obstacles row col currentDirection previousPlaces
  | (row, col) `elem` previousPlaces = True
  | otherwise =
      let nextObstacles = case currentDirection of
            Main.Up -> sortBy (flip compareRows) $ filter ((< row) . fst) $ filter ((== col) . snd) obstacles
            Main.Down -> sortBy compareRows $ filter ((> row) . fst) $ filter ((== col) . snd) obstacles
            Main.Left -> sortBy (flip compareCols) $ filter ((< col) . snd) $ filter ((== row) . fst) obstacles
            Main.Right -> sortBy compareCols $ filter ((> col) . snd) $ filter ((== row) . fst) obstacles
       in ( not (null nextObstacles)
              && ( let (nextObstacleRow, nextObstacleCol) = head nextObstacles
                       (nextRow, nextCol) = case currentDirection of
                         Main.Up -> (nextObstacleRow + 1, nextObstacleCol)
                         Main.Down -> (nextObstacleRow - 1, nextObstacleCol)
                         Main.Left -> (nextObstacleRow, nextObstacleCol + 1)
                         Main.Right -> (nextObstacleRow, nextObstacleCol - 1)
                       nextDirection = case currentDirection of
                         Main.Up -> Main.Right
                         Main.Down -> Main.Left
                         Main.Left -> Main.Up
                         Main.Right -> Main.Down
                    in doesLoop obstacles nextRow nextCol nextDirection (if row == nextRow && col == nextCol then previousPlaces else (row, col) : previousPlaces)
                 )
          )

main = do
  contents <- getContents
  let input = split (== '\n') contents
      inputWithIdx =
        flatten $
          map (\(row_idx, row) -> map (\(col_idx, value) -> (row_idx, col_idx, value)) row) $
            enumerate $
              map enumerate input
      obstacles = map (\(a, b, _) -> (a, b)) $ filter (\(_, _, val) -> val == '#') inputWithIdx
      (startingRow, startingCol, _) =
        head $
          filter (\(_, _, val) -> val == '^') inputWithIdx
      possibleObstacles = moveUp input startingRow startingCol
   in do
        print $ length $ filter (\possibleObstacle -> doesLoop (possibleObstacle : obstacles) startingRow startingCol Main.Up []) possibleObstacles
