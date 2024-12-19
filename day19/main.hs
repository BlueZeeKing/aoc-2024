import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

startMatches :: String -> String -> Bool
startMatches "" _ = True
startMatches _ "" = False
startMatches (expected : expected_remaining) (input : input_remaining) =
  expected == input && startMatches expected_remaining input_remaining

skipWhile :: (a -> Bool) -> [a] -> [a]
skipWhile _ [] = []
skipWhile predicate (start : rest) =
  if predicate start
    then skipWhile predicate rest
    else start : rest

split :: (a -> Bool) -> [a] -> [[a]]
split predicate list =
  let processedList = skipWhile predicate list
      firstGroup = takeWhile (not . predicate) processedList
      remaining = skipWhile (not . predicate) processedList
   in if null processedList then [] else firstGroup : split predicate remaining

skip :: Int -> [a] -> [a]
skip _ [] = []
skip 0 rest = rest
skip amountToSkip (_ : rest) = skip (amountToSkip - 1) rest

numWaysPossible :: Map.Map Int [Int] -> Int -> Int
numWaysPossible towels len =
  head $
    foldr
      ( \idx acc ->
          sum (maybe [] (map (\length -> acc !! (length - 1))) (Map.lookup idx towels)) : acc
      )
      [1]
      [0 .. len - 1]

findOccurrences :: Int -> String -> String -> [Int]
findOccurrences _ _ "" = []
findOccurrences idx expected input
  | startMatches expected input = idx : next
  | otherwise = next
  where
    next = findOccurrences (idx + 1) expected (skip 1 input)

findMatchingSections :: String -> [String] -> [(Int, Int)]
findMatchingSections design towels = foldr1 (++) $ map (\towel -> let idxs = findOccurrences 0 towel design in map (,length towel) idxs) towels

accumulateSections :: [(Int, Int)] -> Map.Map Int [Int]
accumulateSections = foldr (\(key, val) acc -> if Map.member key acc then Map.adjust (val :) key acc else Map.insert key [val] acc) Map.empty

main = do
  contents <- getContents
  let [[rawTowels], designs] = split null $ lines contents
      towels = split (== ',') $ filter (/= ' ') rawTowels
  print $ sum $ map (\design -> numWaysPossible (accumulateSections $ findMatchingSections design towels) (length design)) designs
