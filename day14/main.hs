import qualified Data.Map as Map

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

moveRobot :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
moveRobot ((x, y), (vx, vy)) = (((x + vx) `mod` width, (y + vy) `mod` height), (vx, vy))

applyMultipleTimes :: (a -> a) -> Int -> a -> a
applyMultipleTimes _ 0 value = value
applyMultipleTimes function amount value = applyMultipleTimes function (amount - 1) (function value)

createMap :: [(Int, Int)] -> Map.Map (Int, Int) Int
createMap = foldr (\pos acc -> if pos `Map.member` acc then Map.adjust (+ 1) pos acc else Map.insert pos 1 acc) Map.empty

countSurrounding :: Map.Map (Int, Int) Int -> Int -> Int -> Int
countSurrounding robots x y =
  let surrounding = [(x `mod` width, y `mod` height) | x <- [x - 1 .. x + 1], y <- [y - 1 .. y + 1]]
   in sum $ map (\pos -> Map.findWithDefault 0 pos robots) surrounding

calculateRandomness :: [(Int, Int)] -> Int
calculateRandomness robots =
  let robotMap = createMap robots
   in sum $ map (uncurry (countSurrounding robotMap)) robots

loop :: [((Int, Int), (Int, Int))] -> Int -> IO ()
loop robots 0 =
  let emptyBoard = replicate height $ replicate width '.'
      robotPositions = map fst robots
      setRobot (x, y) board =
        let firstRows = take y board
            afterRows = skip (y + 1) board
            currentRow = board !! y
            firstCols = take x currentRow
            afterCols = skip (x + 1) currentRow
         in firstRows ++ [firstCols ++ "#" ++ afterCols] ++ afterRows
   in putStrLn $ foldr1 (\fst snd -> fst ++ '\n' : snd) $ foldr setRobot emptyBoard robotPositions
loop robots iteration = do
  let randomness = calculateRandomness $ map fst robots
  putStrLn (show iteration ++ "," ++ show randomness)
  loop (map moveRobot robots) (iteration - 1)

main = do
  contents <- getContents
  let robots :: [((Int, Int), (Int, Int))] = map ((\[a, b] -> (a, b)) . map ((\[a, b] -> (a, b)) . map read . split (== ',') . skip 2) . split (== ' ')) $ lines contents
  -- putStrLn "iteration,randomness"
  loop robots 100000
