{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

import qualified Data.Map as Map
import Data.Maybe

enumerateInner :: Int -> [a] -> [(Int, a)]
enumerateInner _ [] = []
enumerateInner idx (first : remaining) = (idx, first) : enumerateInner (idx + 1) remaining

enumerate :: [a] -> [(Int, a)]
enumerate = enumerateInner 0

flatten :: [[a]] -> [a]
flatten [] = []
flatten [[]] = []
flatten ([] : (next : innerRemaining) : remaining) = next : flatten (innerRemaining : remaining)
flatten ((next : innerRemaining) : remaining) = next : flatten (innerRemaining : remaining)

skipWhile :: (a -> Bool) -> [a] -> [a]
skipWhile _ [] = []
skipWhile shouldSkip (start : rest) =
  if shouldSkip start
    then skipWhile shouldSkip rest
    else start : rest

split :: (a -> Bool) -> [a] -> [[a]]
split shouldSplit list =
  let processedList = skipWhile shouldSplit list
      firstGroup = takeWhile (not . shouldSplit) processedList
      remaining = skipWhile (not . shouldSplit) processedList
   in if null processedList then [] else firstGroup : split shouldSplit remaining

trd :: (a, b, c) -> c
trd (_, _, val) = val

data Cell = Box | Border | Robot | BoxLeft | BoxRight
  deriving (Show, Eq)

data Direction = Up | Down | Left | Right
  deriving (Show, Eq)

applyDirection :: Direction -> Int -> Int -> (Int, Int)
applyDirection Main.Up row col = (row - 1, col)
applyDirection Main.Down row col = (row + 1, col)
applyDirection Main.Left row col = (row, col - 1)
applyDirection Main.Right row col = (row, col + 1)

mergeMaybe :: Maybe (Maybe a) -> Maybe a
mergeMaybe (Just (Just val)) = Just val
mergeMaybe _ = Nothing

attemptMove :: Map.Map (Int, Int) Cell -> Int -> Int -> Direction -> Maybe (Map.Map (Int, Int) Cell)
attemptMove cells row col direction
  | isNothing currentCell = Just cells
  | fromJust currentCell == Border = Nothing
  | fromJust currentCell == BoxLeft = attemptMove cells row (col + 1) direction
  | fromJust currentCell == Robot = Map.insert (nextRow, nextCol) Robot . Map.delete (row, col) <$> attemptMove cells nextRow nextCol direction
  | fromJust currentCell == BoxRight && (direction == Main.Up || direction == Main.Down) =
      Map.insert (nextRow, nextCol) BoxRight
        . Map.insert (nextRow, nextCol - 1) BoxLeft
        . Map.delete (row, col - 1)
        . Map.delete (row, col)
        <$> mergeMaybe
          ( (\newCells -> attemptMove newCells nextRow (nextCol - 1) direction)
              <$> attemptMove cells nextRow nextCol direction
          )
  | fromJust currentCell == BoxRight =
      Map.insert (nextRow, nextCol) BoxRight
        . Map.insert (nextRow, nextCol - 1) BoxLeft
        . Map.delete (row, col - 1)
        . Map.delete (row, col)
        <$> attemptMove cells nextRow (if direction == Main.Right then nextCol else nextCol - 1) direction
  where
    currentCell = Map.lookup (row, col) cells
    (nextRow, nextCol) = applyDirection direction row col

applyDirections :: Map.Map (Int, Int) Cell -> [Direction] -> Map.Map (Int, Int) Cell
applyDirections cells [] = cells
applyDirections cells (val : remaining) =
  let (robotRow, robotCol) = fst $ head $ filter ((== Robot) . snd) $ Map.toList cells
      newCells = fromMaybe cells $ attemptMove cells robotRow robotCol val
   in applyDirections newCells remaining

skip :: Int -> [a] -> [a]
skip _ [] = []
skip 0 rest = rest
skip amountToSkip (_ : rest) = skip (amountToSkip - 1) rest

displayBoard :: Map.Map (Int, Int) Cell -> Int -> Int -> String
displayBoard cells width height =
  let emptyBoard = replicate height $ replicate width '.'
      setRobot ((y, x), cell) board =
        let firstRows = take y board
            afterRows = skip (y + 1) board
            currentRow = board !! y
            firstCols = take x currentRow
            afterCols = skip (x + 1) currentRow
            letter = case cell of
              Box -> 'O'
              BoxLeft -> '['
              BoxRight -> ']'
              Border -> '#'
              Robot -> '@'
         in firstRows ++ [firstCols ++ [letter] ++ afterCols] ++ afterRows
   in foldr1 (\fst snd -> fst ++ '\n' : snd) $ foldr setRobot emptyBoard $ Map.toList cells

main = do
  contents <- getContents
  let [warehouse, splitDirections] = split null $ lines contents
      directions =
        map
          ( \dir -> case dir of
              '>' -> Main.Right
              '<' -> Main.Left
              '^' -> Main.Up
              'v' -> Main.Down
          )
          $ foldr1 (++) splitDirections
      thinCellsList =
        map
          ( \(rowIdx, colIdx, value) ->
              ( rowIdx,
                colIdx,
                case value of
                  '@' -> Robot
                  '#' -> Border
                  'O' -> Box
              )
          )
          $ filter
            ((/= '.') . trd)
          $ flatten
          $ map (\(rowIdx, row) -> map (\(colIdx, value) -> (rowIdx, colIdx, value)) row)
          $ enumerate
          $ map enumerate warehouse
      thickCellsList =
        flatten $
          map
            ( \(rowIdx, colIdx, value) -> case value of
                Robot -> [(rowIdx, colIdx * 2, Robot)]
                Border -> [(rowIdx, colIdx * 2, Border), (rowIdx, colIdx * 2 + 1, Border)]
                Box -> [(rowIdx, colIdx * 2, BoxLeft), (rowIdx, colIdx * 2 + 1, BoxRight)]
            )
            thinCellsList
      initialCells = Map.fromList $ map (\(rowIdx, colIdx, value) -> ((rowIdx, colIdx), value)) thickCellsList
      finalPosition = applyDirections initialCells directions
  putStrLn $ displayBoard (applyDirections initialCells directions) (length (head warehouse) * 2) (length warehouse)
  print $ sum $ map ((\(row, col) -> row * 100 + col) . fst) $ filter ((== BoxLeft) . snd) $ Map.toList finalPosition
