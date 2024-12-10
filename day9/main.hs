import Data.Binary.Get (remaining)
import Debug.Trace
import Distribution.Utils.Generic (fstOf3, sndOf3)
import Distribution.Utils.String (trim)

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

enumerate :: [a] -> [(Int, a)]
enumerate [] = []
enumerate (first : remaining) = (0, first) : map (\(idx, val) -> (idx + 1, val)) (enumerate remaining)

flatten :: [[a]] -> [a]
flatten [[]] = []
flatten ([] : (next : innerRemaining) : remaining) = next : flatten (innerRemaining : remaining)
flatten ((next : innerRemaining) : remaining) = next : flatten (innerRemaining : remaining)

parse :: String -> [(Int, Int, Int)]
parse [last] = [(0, charToInt last, 0)]
parse (amountFull : amountEmpty : remaining) = (0, charToInt amountFull, charToInt amountEmpty) : map (\(idx, full, empty) -> (idx + 1, full, empty)) (parse remaining)

insertFile :: Int -> Int -> [(Int, Int, Int)] -> Maybe [(Int, Int, Int)]
insertFile idx size [] = Nothing
insertFile currentIndex amountFull ((testIdx, testFull, testEmpty) : remaining) =
  if testEmpty >= amountFull
    then Just $ (testIdx, testFull, 0) : (currentIndex, amountFull, testEmpty - amountFull) : remaining
    else ((testIdx, testFull, testEmpty) :) <$> insertFile currentIndex amountFull remaining

skipUntilAndIncluding :: (a -> Bool) -> [a] -> [a]
skipUntilAndIncluding _ [] = []
skipUntilAndIncluding shouldSkip (val : remaining) = if shouldSkip val then remaining else skipUntilAndIncluding shouldSkip remaining

splitAroundFile :: Int -> [(Int, Int, Int)] -> ([(Int, Int, Int)], (Int, Int, Int), [(Int, Int, Int)])
splitAroundFile idx input = (takeWhile ((/= idx) . fstOf3) input, head $ filter ((== idx) . fstOf3) input, skipUntilAndIncluding ((== idx) . fstOf3) input)

placeFile :: Int -> [(Int, Int, Int)] -> [(Int, Int, Int)]
placeFile idx input =
  let (start, (_, size, empty), end) = splitAroundFile idx input
   in case insertFile idx size start of
        Nothing -> input
        Just newStart ->
          let startNewStart = init newStart
              (lastIdx, lastSize, lastEmpty) = last newStart
           in startNewStart ++ [(lastIdx, lastSize, lastEmpty + size + empty)] ++ end

calculateDiskMap :: [(Int, Int, Int)] -> [(Int, Int, Int)]
calculateDiskMap input =
  let range = [1 .. length input - 1]
   in foldr placeFile input range

calculateCheckSum :: [(Int, Int, Int)] -> Int -> Int
calculateCheckSum [] _ = 0
calculateCheckSum ((idx, size, empty) : remaining) startingPos = sum (map (* idx) $ take size [startingPos ..]) + calculateCheckSum remaining (startingPos + size + empty)

main = do
  contents <- getContents
  let values = calculateDiskMap $ parse $ trim contents
   in print $ calculateCheckSum values 0
