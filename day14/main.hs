width = 101

height = 103

skipWhile :: (a -> Bool) -> [a] -> [a]
skipWhile _ [] = []
skipWhile predicate (start : rest) =
  if predicate start
    then skipWhile predicate rest
    else start : rest

split :: (a -> Bool) -> [a] -> [[a]]
split shouldSplit list =
  let processedList = skipWhile shouldSplit list
      firstGroup = takeWhile (not . shouldSplit) processedList
      remaining = skipWhile (not . shouldSplit) processedList
   in if null processedList then [] else firstGroup : split shouldSplit remaining

skip :: Int -> [a] -> [a]
skip _ [] = []
skip 0 rest = rest
skip amountToSkip (_ : rest) = skip (amountToSkip - 1) rest

moveRobot :: Int -> ((Int, Int), (Int, Int)) -> (Int, Int)
moveRobot times ((x, y), (vx, vy)) = ((x + vx * times) `mod` width, (y + vy * times) `mod` height)

applyMultipleTimes :: (a -> a) -> Int -> a -> a
applyMultipleTimes _ 0 value = value
applyMultipleTimes function amount value = applyMultipleTimes function (amount - 1) (function value)

main = do
  contents <- getContents
  let robots :: [((Int, Int), (Int, Int))] = map ((\[a, b] -> (a, b)) . map ((\[a, b] -> (a, b)) . map read . split (== ',') . skip 2) . split (== ' ')) $ lines contents
      finalRobotsPos = map (moveRobot 100) robots
      left = filter ((< (width `div` 2)) . fst) finalRobotsPos
      right = filter ((> (width `div` 2)) . fst) finalRobotsPos
      topLeft = filter ((< (height `div` 2)) . snd) left
      bottomLeft = filter ((> (height `div` 2)) . snd) left
      topRight = filter ((< (height `div` 2)) . snd) right
      bottomRight = filter ((> (height `div` 2)) . snd) right
  print $ product $ map length [topLeft, bottomLeft, topRight, bottomRight]
