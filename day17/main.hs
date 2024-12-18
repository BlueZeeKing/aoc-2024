import Data.Bits
import Data.List (minimumBy)
import Data.Maybe (fromJust, mapMaybe)
import Debug.Trace

skip :: Int -> [a] -> [a]
skip _ [] = []
skip 0 rest = rest
skip amountToSkip (_ : rest) = skip (amountToSkip - 1) rest

skipWhile :: (a -> Bool) -> [a] -> [a]
skipWhile _ [] = []
skipWhile shouldSkip (start : rest) =
  if shouldSkip start
    then skipWhile shouldSkip rest
    else start : rest

split :: (a -> Bool) -> [a] -> [[a]]
split shouldSplit list =
  let processedList = skipWhile shouldSplit list
      firstGroup = takeWhile (not . shouldSplit) processedList
      remaining = skipWhile (not . shouldSplit) processedList
   in if null processedList then [] else firstGroup : split shouldSplit remaining

numDigits :: Int -> Int
numDigits val =
  let digits = logBase 10 $ fromIntegral val
   in floor $ digits + 1

runComputer :: [Int] -> Int -> Int -> Int -> Int -> [Int]
runComputer program pc a b c
  | pc + 1 > length program = []
  | otherwise =
      let instruction = program !! pc
          operand = program !! (pc + 1)
          comboOperand = case operand of
            0 -> 0
            1 -> 1
            2 -> 2
            3 -> 3
            4 -> a
            5 -> b
            6 -> c
            7 -> error "bad stuff"
       in case instruction of
            0 -> runComputer program (pc + 2) (a `div` (2 ^ comboOperand)) b c
            1 -> runComputer program (pc + 2) a (b `xor` operand) c
            2 -> runComputer program (pc + 2) a (comboOperand `mod` 8) c
            3 -> if a == 0 then runComputer program (pc + 2) a b c else runComputer program operand a b c
            4 -> runComputer program (pc + 2) a (b `xor` c) c
            5 -> comboOperand `mod` 8 : runComputer program (pc + 2) a b c
            6 -> runComputer program (pc + 2) a (a `div` (2 ^ comboOperand)) c
            7 -> runComputer program (pc + 2) a b (a `div` (2 ^ comboOperand))

findA :: [Int] -> Int -> Int -> [[Int]]
findA [] a _ = [[a]]
findA (rawTarget : remaining) a aMap =
  let target = rawTarget `xor` 6
      filterAMod8 aMod8 = aMod8 .&. (aMap .&. 7) == a .&. (aMap .&. 7)
      filterShiftedValue aMod8 =
        let shiftedAmount = aMod8 `xor` 3
            requiredShiftedValue = target `xor` aMod8
            actualShiftedValue = a `shiftR` shiftedAmount
            shiftedMap = (aMap `shiftR` shiftedAmount) .&. 7
         in requiredShiftedValue .&. shiftedMap == actualShiftedValue .&. shiftedMap
      results =
        flatten
          $ filter (not . null)
          $ map
            ( \aMod8 ->
                let shiftAmount = aMod8 `xor` 3
                    requiredShiftedValue = target `xor` aMod8
                    newA = a .|. (requiredShiftedValue `shiftL` shiftAmount)
                    newAMap = aMap .|. (7 `shiftL` shiftAmount)
                 in map (aMod8 :) $ findA remaining (newA `shiftR` 3) (newAMap `shiftR` 3)
            )
          $ filter
            (\aMod8 -> filterAMod8 aMod8 && filterShiftedValue aMod8)
            [0 .. 7]
   in results

flatten :: [[a]] -> [a]
flatten [] = []
flatten [[]] = []
flatten ([] : (next : innerRemaining) : remaining) = next : flatten (innerRemaining : remaining)
flatten ((next : innerRemaining) : remaining) = next : flatten (innerRemaining : remaining)

enumerate :: [a] -> [(Int, a)]
enumerate [] = []
enumerate (first : remaining) = (0, first) : map (\(idx, val) -> (idx + 1, val)) (enumerate remaining)

main = do
  contents <- getContents
  let [rawA, rawB, rawC, rawProgram] = filter (not . null) $ lines contents
      b :: Int = read $ skip 12 rawB
      c :: Int = read $ skip 12 rawC
      program :: [Int] = map read $ split (== ',') $ skip 9 rawProgram
      result = filter (\a -> runComputer program 0 a b c == program) $ map (sum . map (\(idx, val) -> val `shiftL` (idx * 3)) . enumerate) $ findA program 0 0
  print $ minimum result
