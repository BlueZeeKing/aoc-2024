import Debug.Trace
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

parse :: String -> [(Int, Int, Int)]
parse [last] = [(0, charToInt last, 0)]
parse (amountFull : amountEmpty : remaining) = (0, charToInt amountFull, charToInt amountEmpty) : map (\(idx, full, empty) -> (idx + 1, full, empty)) (parse remaining)

fillEmptySpace :: [(Int, Int, Int)] -> Int -> ([Int], [(Int, Int, Int)])
fillEmptySpace [] _ = ([], [])
fillEmptySpace files 0 = ([], files)
fillEmptySpace files amountToFill =
  let (currentIdx, amountFull, _) = last files
   in if amountFull >= amountToFill
        then (replicate amountToFill currentIdx, init files ++ if amountFull == amountToFill then [] else [(currentIdx, amountFull - amountToFill, 0)])
        else
          let (nextIds, nextFiles) = fillEmptySpace (init files) (amountToFill - amountFull)
           in (replicate amountFull currentIdx ++ nextIds, nextFiles)

calculateDiskMap :: [(Int, Int, Int)] -> [Int]
calculateDiskMap [] = []
calculateDiskMap ((currentIdx, amountFull, amountEmpty) : remaining) =
  let firstPart = replicate amountFull currentIdx
      (fillPart, newRemaining) = fillEmptySpace remaining amountEmpty
   in firstPart ++ fillPart ++ calculateDiskMap newRemaining

main = do
  contents <- getContents
  print $ sum $ map (uncurry (*)) $ enumerate $ calculateDiskMap $ parse $ trim contents
