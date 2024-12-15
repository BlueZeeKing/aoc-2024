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

data Cell = Box | Border | Robot
  deriving (Show, Eq)

data Direction = Up | Down | Left | Right
  deriving (Show, Eq)

applyDirection :: Direction -> Int -> Int -> (Int, Int)
applyDirection Main.Up row col = (row - 1, col)
applyDirection Main.Down row col = (row + 1, col)
applyDirection Main.Left row col = (row, col - 1)
applyDirection Main.Right row col = (row, col + 1)

attemptMove :: Map.Map (Int, Int) Cell -> Int -> Int -> Direction -> Maybe (Map.Map (Int, Int) Cell)
attemptMove cells row col direction
  | isNothing currentCell = Just cells
  | fromJust currentCell == Border = Nothing
  | fromJust currentCell == Box || fromJust currentCell == Robot = Map.insert (nextRow, nextCol) (fromJust currentCell) . Map.delete (row, col) <$> attemptMove cells nextRow nextCol direction
  where
    currentCell = Map.lookup (row, col) cells
    (nextRow, nextCol) = applyDirection direction row col

applyDirections :: Map.Map (Int, Int) Cell -> [Direction] -> Map.Map (Int, Int) Cell
applyDirections cells [] = cells
applyDirections cells (val : remaining) =
  let (robotRow, robotCol) = fst $ head $ filter ((== Robot) . snd) $ Map.toList cells
      newCells = fromMaybe cells $ attemptMove cells robotRow robotCol val
   in applyDirections newCells remaining

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
      cellsList =
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
      initialCells = Map.fromList $ map (\(rowIdx, colIdx, value) -> ((rowIdx, colIdx), value)) cellsList
      finalPosition = applyDirections initialCells directions
  print $ sum $ map ((\(row, col) -> row * 100 + col) . fst) $ filter ((== Box) . snd) $ Map.toList finalPosition
