numDigits :: Int -> Int
numDigits val =
  let digits = logBase 10 $ fromIntegral val
   in floor $ digits + 1

splitByDigits :: Int -> (Int, Int)
splitByDigits val =
  let digits = numDigits val `div` 2
   in (val `div` (10 ^ digits), val `mod` (10 ^ digits))

updateRocks :: [Int] -> [Int]
updateRocks [] = []
updateRocks (0 : remaining) = 1 : updateRocks remaining
updateRocks (val : remaining)
  | even $ numDigits val = let (first, second) = splitByDigits val in first : second : updateRocks remaining
  | otherwise = val * 2024 : updateRocks remaining

main = do
  contents <- getContents
  let input :: [Int] = map read $ words contents
  print $ length $ foldr (\_ val -> updateRocks val) input [1 .. 25]
