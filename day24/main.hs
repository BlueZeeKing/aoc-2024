import Data.List (sort)
import qualified Data.Map as Map

enumerateInner :: Int -> [a] -> [(Int, a)]
enumerateInner _ [] = []
enumerateInner idx (first : remaining) = (idx, first) : enumerateInner (idx + 1) remaining

enumerate :: [a] -> [(Int, a)]
enumerate = enumerateInner 0

skipWhile :: (a -> Bool) -> [a] -> [a]
skipWhile _ [] = []
skipWhile shouldSkip (start : rest) =
  if shouldSkip start
    then skipWhile shouldSkip rest
    else start : rest

flatten :: [[a]] -> [a]
flatten = foldr1 (++)

split :: (a -> Bool) -> [a] -> [[a]]
split shouldSplit list =
  let processedList = skipWhile shouldSplit list
      firstGroup = takeWhile (not . shouldSplit) processedList
      remaining = skipWhile (not . shouldSplit) processedList
   in if null processedList then [] else firstGroup : split shouldSplit remaining

data Gate = And | Or | Xor
  deriving (Show)

applyGate :: Gate -> Bool -> Bool -> Bool
applyGate And fst snd = fst && snd
applyGate Or fst snd = fst || snd
applyGate Xor fst snd = fst /= snd

calculateValues :: Map.Map String (String, Gate, String) -> Map.Map String Bool -> [String] -> Map.Map String Bool
calculateValues _ knowns [] = knowns
calculateValues gates knowns (next : remaining)
  | next `Map.member` knowns = calculateValues gates knowns remaining
  | Just fstVal <- fstVar `Map.lookup` knowns, Just sndVal <- sndVar `Map.lookup` knowns = calculateValues gates (Map.insert next (applyGate operator fstVal sndVal) knowns) remaining
  | otherwise = calculateValues gates knowns (fstVar : sndVar : next : remaining)
  where
    (fstVar, operator, sndVar) = gates Map.! next

main = do
  contents <- getContents
  let [initialValsRaw, rawGates] = split null $ lines contents
      initials =
        map
          ( \val ->
              let [var, value] = words val
               in (take 3 var, value == "1")
          )
          initialValsRaw
      gates =
        Map.fromList $
          map
            ( \gate ->
                let [fstVar, operator, sndVar, "->", outputVar] = words gate
                 in ( outputVar,
                      ( fstVar,
                        case operator of
                          "AND" -> And
                          "OR" -> Or
                          "XOR" -> Xor,
                        sndVar
                      )
                    )
            )
            rawGates
      zs = sort $ filter ((== 'z') . head) $ map fst $ Map.toList gates
      vals = calculateValues gates (Map.fromList initials) zs
  print $ sum $ map ((2 ^) . fst) $ filter snd $ enumerate $ map (vals Map.!) zs
