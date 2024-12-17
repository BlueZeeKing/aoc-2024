import Data.Bits (xor)
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

main = do
  contents <- getContents
  let [rawA, rawB, rawC, rawProgram] = filter (not . null) $ lines contents
      a :: Int = read $ skip 12 rawA
      b :: Int = read $ skip 12 rawB
      c :: Int = read $ skip 12 rawC
      program :: [Int] = map read $ split (== ',') $ skip 9 rawProgram
      result = foldr1 (\a b -> a ++ "," ++ b) $ map show $ runComputer program 0 a b c
  print result
