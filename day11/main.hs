import Data.List

numDigits :: Int -> Int
numDigits val =
  let digits = logBase 10 $ fromIntegral val
   in floor $ digits + 1

splitByDigits :: Int -> (Int, Int)
splitByDigits val =
  let digits = numDigits val `div` 2
   in (val `div` (10 ^ digits), val `mod` (10 ^ digits))

updateRock :: Int -> [Int]
updateRock val
  | val == 0 = [1]
  | even $ numDigits val = let (first, second) = splitByDigits val in [first, second]
  | otherwise = [val * 2024]

split :: (a -> Bool) -> [a] -> ([a], [a])
split _ [] = ([], [])
split predicate (start : rest) =
  if predicate start
    then let (first, second) = split predicate rest in (start : first, second)
    else ([], start : rest)

groupRocks :: [(Int, Int)] -> [(Int, Int)]
groupRocks [] = []
groupRocks ((amount, value) : remaining) =
  let (firstPart, secondPart) = split ((== value) . snd) remaining
   in (amount + sum (map fst firstPart), value) : groupRocks secondPart

mergeRocks :: [(Int, Int)] -> [(Int, Int)]
mergeRocks input =
  let sorted = sortBy (\(_, a) (_, b) -> a `compare` b) input
   in groupRocks sorted

flatten :: [[a]] -> [a]
flatten [] = []
flatten [[]] = []
flatten ([] : (next : innerRemaining) : remaining) = next : flatten (innerRemaining : remaining)
flatten ((next : innerRemaining) : remaining) = next : flatten (innerRemaining : remaining)

updateRocks :: [(Int, Int)] -> [(Int, Int)]
updateRocks rocks =
  let updatedRocks = flatten $ map (\(amount, value) -> map (amount,) $ updateRock value) rocks
   in mergeRocks updatedRocks

main = do
  contents <- getContents
  let input :: [Int] = map read $ words contents
  print $ sum $ map fst $ foldr (\_ val -> updateRocks val) (map (1,) input) [1 .. 75]
