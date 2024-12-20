import Data.List (sort)
import qualified Data.Map as Map

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

bfs :: Map.Map (Int, Int) (Maybe (Int, Int)) -> [(Int, Int)] -> Map.Map (Int, Int) (Maybe (Int, Int))
bfs values [] = values
bfs barriersOrVisited ((x, y) : remaining) =
  let surrounding = filter (`Map.notMember` barriersOrVisited) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
   in bfs (foldr (`Map.insert` Just (x, y)) barriersOrVisited surrounding) (remaining ++ surrounding)

findPath :: Map.Map (Int, Int) (Maybe (Int, Int)) -> Int -> Int -> [(Int, Int)]
findPath values row col
  | Just (previousRow, previousCol) <- values Map.! (row, col) = (row, col) : findPath values previousRow previousCol
  | otherwise = [(row, col)]

canSkip :: Int -> Int -> Int -> Int -> Bool
canSkip fstRow fstCol sndRow sndCol = abs (fstRow - sndRow) + abs (fstCol - sndCol) == 2

combinationsOf2 :: [a] -> [(a, a)]
combinationsOf2 [] = []
combinationsOf2 (first : remaining) = map (first,) remaining ++ combinationsOf2 remaining

main = do
  contents <- getContents
  let cells =
        flatten $
          map (\(rowIdx, row) -> map (\(colIdx, value) -> (rowIdx, colIdx, value)) row) $
            enumerate $
              map enumerate $
                lines contents
      barriers = map (\(a, b, _) -> ((a, b), Nothing)) $ filter ((== '#') . trd) cells
      (startingRow, startingCol, _) = head $ filter ((== 'S') . trd) cells
      (endingRow, endingCol, _) = head $ filter ((== 'E') . trd) cells

      values = bfs (Map.insert (startingRow, startingCol) Nothing $ Map.fromList barriers) [(startingRow, startingCol)]
      path = enumerate $ findPath values endingRow endingCol
      shortcuts =
        map (\((fstIdx, (fstRow, fstCol)), (sndIdx, (sndRow, sndCol))) -> (abs (sndIdx - fstIdx) - (abs (fstRow - sndRow) + abs (fstCol - sndCol)), (fstRow, fstCol), (sndRow, sndCol))) $
          filter (\((_, (fstRow, fstCol)), (_, (sndRow, sndCol))) -> canSkip fstRow fstCol sndRow sndCol) $
            combinationsOf2 path
  print $ length $ filter (\(dist, _, _) -> dist >= 100) shortcuts
