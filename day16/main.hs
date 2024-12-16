{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
import Data.List (minimumBy)
import qualified Data.Map as Map
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Set as Set
import Debug.Trace (trace)

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

trd :: (a, b, c) -> c
trd (_, _, val) = val

data Direction = North | South | East | West
  deriving (Eq, Show, Ord)

applyDirection :: Direction -> Int -> Int -> (Int, Int)
applyDirection North row col = (row - 1, col)
applyDirection South row col = (row + 1, col)
applyDirection West row col = (row, col - 1)
applyDirection East row col = (row, col + 1)

getOtherDirections :: Direction -> [Direction]
getOtherDirections North = [West, East]
getOtherDirections South = [West, East]
getOtherDirections East = [North, South]
getOtherDirections West = [North, South]

getOtherNodes :: Map.Map (Int, Int, Direction) b -> (Int, Int, Direction) -> [((Int, Int, Direction), Int)]
getOtherNodes nodes (rowIdx, colIdx, dir) =
  let otherDirections = getOtherDirections dir
      (nextRowIdx, nextColIdx) = applyDirection dir rowIdx colIdx
      possibleNodes = ((nextRowIdx, nextColIdx, dir), 1) : map (\dir -> ((rowIdx, colIdx, dir), 1000)) otherDirections
   in filter ((`Map.member` nodes) . fst) possibleNodes

dijkstra :: Map.Map (Int, Int, Direction) (Maybe Int, [(Int, Int, Direction)]) -> Int -> Int -> [((Int, Int, Direction), (Maybe Int, [(Int, Int, Direction)]))]
dijkstra unvisited endRow endCol =
  let ((nextVisitedRow, nextVisitedCol, nextVisitedDir), (cost, route)) =
        minimumBy
          ( \(_, (fst, _)) (_, (snd, _)) -> case (fst, snd) of
              (Just fst, Just snd) -> fst `compare` snd
              (Just _, _) -> LT
              (_, Just _) -> GT
              (_, _) -> EQ
          )
          $ Map.toList unvisited
      adjacentNodes = getOtherNodes unvisited (nextVisitedRow, nextVisitedCol, nextVisitedDir)
      nextUnvisted =
        foldr
          ( \(item, additionalCost) acc ->
              Map.adjust
                ( \(oldCost, route) -> case oldCost of
                    Just val ->
                      let newCost = fromJust cost + additionalCost
                       in case newCost `compare` val of
                            LT -> (Just newCost, [(nextVisitedRow, nextVisitedCol, nextVisitedDir)])
                            GT -> (Just val, route)
                            EQ -> (Just val, (nextVisitedRow, nextVisitedCol, nextVisitedDir) : route)
                    Nothing -> (Just (fromJust cost + additionalCost), [(nextVisitedRow, nextVisitedCol, nextVisitedDir)])
                )
                item
                acc
          )
          (Map.delete (nextVisitedRow, nextVisitedCol, nextVisitedDir) unvisited)
          adjacentNodes
   in if null nextUnvisted then [] else ((nextVisitedRow, nextVisitedCol, nextVisitedDir), (cost, route)) : dijkstra nextUnvisted endRow endCol

traceGraph :: Map.Map (Int, Int, Direction) (Maybe Int, [(Int, Int, Direction)]) -> Int -> Int -> Direction -> [(Int, Int)]
traceGraph graph currentRow currentCol direction =
  let (_, nextDirections) = graph Map.! (currentRow, currentCol, direction)
   in (currentCol, currentRow) : flatten (map (\(row, col, dir) -> traceGraph graph row col dir) nextDirections)

main = do
  contents <- getContents
  let cells =
        flatten $
          map (\(rowIdx, row) -> map (\(colIdx, value) -> (rowIdx, colIdx, value)) row) $
            enumerate $
              map enumerate $
                lines contents
      nodes = flatten $ map ((\(rowIdx, colIdx) -> map (rowIdx,colIdx,) [North, East, South, West]) . (\(a, b, _) -> (a, b))) $ filter ((/= '#') . trd) cells
      (startRow, startCol) = head $ map (\(a, b, _) -> (a, b)) $ filter ((== 'S') . trd) cells
      (endRow, endCol) = head $ map (\(a, b, _) -> (a, b)) $ filter ((== 'E') . trd) cells

      unvisitedList = Map.adjust (const (Just 0, [])) (startRow, startCol, East) $ Map.fromList $ map (,(Nothing, [])) nodes

      finalGraph = Map.fromList $ dijkstra unvisitedList endRow endCol

      possibleFinalEndPoints = filter (`Map.member` finalGraph) $ map (endRow,endCol,) [North, East, South, West]
      finalScore = minimum $ mapMaybe (fst . (finalGraph Map.!)) possibleFinalEndPoints
      finalEndPoints = filter ((Just finalScore ==) . fst . (finalGraph Map.!)) possibleFinalEndPoints
      routes = foldr (\(row, col, dir) acc -> traceGraph finalGraph row col dir ++ acc) [] finalEndPoints
  print $ length $ Set.fromList routes
