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

calculatePerimeter :: [(Int, Int)] -> Int
calculatePerimeter region = sum $ map (uncurry (calculatePerimeterAtPoint region)) region

calculatePrice :: [(Int, Int)] -> Int
calculatePrice region = length region * calculatePerimeter region

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
  let points =
        flatten $
          map (\(row_idx, row) -> map (\(col_idx, value) -> (row_idx, col_idx)) row) $
            enumerate $
              map enumerate squares
  print $ let !val = priceForAllRegions squares points in val
