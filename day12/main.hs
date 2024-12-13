import Data.List

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

findRegion :: [String] -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
findRegion squares row col visited =
  let width = length $ head squares
      height = length squares
      borderingSides = [(row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)]
      currentVegetable = squares !! row !! col
      validBorderingSides = filter (\(row, col) -> row >= 0 && col >= 0 && row < height && col < width && (row, col) `notElem` visited && squares !! row !! col == currentVegetable) borderingSides
      newVisited = validBorderingSides ++ visited
   in foldr (\(row, col) newVisited -> findRegion squares row col newVisited) newVisited validBorderingSides

calculatePerimeterAtPoint :: [(Int, Int)] -> Int -> Int -> Int
calculatePerimeterAtPoint region row col =
  let borderingSides = [(row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)]
   in length $ filter (\(row, col) -> (row, col) `notElem` region) borderingSides

countGroups :: (a -> a -> Bool) -> [a] -> Int
countGroups _ [] = 0
countGroups _ [_] = 1
countGroups sameGroup (first : second : remaining) = (if sameGroup first second then 0 else 1) + countGroups sameGroup (second : remaining)

-- up down left right
findPerimeterInner :: [(Int, Int)] -> [(Int, Int)] -> ([(Int, Int)], [(Int, Int)], [(Int, Int)], [(Int, Int)])
findPerimeterInner _ [] = ([], [], [], [])
findPerimeterInner region ((currentRow, currentCol) : remaining) =
  let (nextUp, nextDown, nextLeft, nextRight) = findPerimeterInner region remaining
   in ( if (currentRow - 1, currentCol) `notElem` region then (currentRow, currentCol) : nextUp else nextUp,
        if (currentRow + 1, currentCol) `notElem` region then (currentRow, currentCol) : nextDown else nextDown,
        if (currentRow, currentCol - 1) `notElem` region then (currentRow, currentCol) : nextLeft else nextLeft,
        if (currentRow, currentCol + 1) `notElem` region then (currentRow, currentCol) : nextRight else nextRight
      )

findPerimeter :: [(Int, Int)] -> ([(Int, Int)], [(Int, Int)], [(Int, Int)], [(Int, Int)])
findPerimeter val = findPerimeterInner val val

calculateSides :: [(Int, Int)] -> Int
calculateSides region =
  let (up, down, left, right) = findPerimeter region
      orderByCol (firstRow, firstCol) (secondRow, secondCol) =
        let colOrdering = firstCol `compare` secondCol
         in if colOrdering == EQ
              then firstRow `compare` secondRow
              else colOrdering
      orderByRow (firstRow, firstCol) (secondRow, secondCol) =
        let rowOrdering = firstRow `compare` secondRow
         in if rowOrdering == EQ
              then firstCol `compare` secondCol
              else rowOrdering

      sortedUp = sortBy orderByRow up
      sortedDown = sortBy orderByRow down
      sortedLeft = sortBy orderByCol left
      sortedRight = sortBy orderByCol right

      numSidesUp = countGroups (\(firstRow, firstCol) (secondRow, secondCol) -> firstRow == secondRow && abs (firstCol - secondCol) == 1) sortedUp
      numSidesDown = countGroups (\(firstRow, firstCol) (secondRow, secondCol) -> firstRow == secondRow && abs (firstCol - secondCol) == 1) sortedDown
      numSidesLeft = countGroups (\(firstRow, firstCol) (secondRow, secondCol) -> firstCol == secondCol && abs (firstRow - secondRow) == 1) sortedLeft
      numSidesRight = countGroups (\(firstRow, firstCol) (secondRow, secondCol) -> firstCol == secondCol && abs (firstRow - secondRow) == 1) sortedRight
   in numSidesUp + numSidesDown + numSidesLeft + numSidesRight

calculatePrice :: [(Int, Int)] -> Int
calculatePrice region = length region * calculateSides region

priceForAllRegions :: [String] -> [(Int, Int)] -> Int
priceForAllRegions _ [] = 0
priceForAllRegions squares points =
  let (row, col) = head points
      region = findRegion squares row col [(row, col)]
      remainingPoints = filter (`notElem` region) points
   in calculatePrice region + priceForAllRegions squares remainingPoints

main = do
  contents <- getContents
  let squares = lines contents
      points =
        flatten $
          map (\(row_idx, row) -> map (\(col_idx, value) -> (row_idx, col_idx)) row) $
            enumerate $
              map enumerate squares
  print $ priceForAllRegions squares points
