import Data.Char
import Debug.Trace

skipWhile :: (a -> Bool) -> [a] -> [a]
skipWhile _ [] = []
skipWhile predicate (start : rest) =
  if predicate start
    then skipWhile predicate rest
    else start : rest

skip :: Int -> [a] -> [a]
skip _ [] = []
skip 0 rest = rest
skip amount_to_skip (_ : rest) = skip (amount_to_skip - 1) rest

split :: (a -> Bool) -> [a] -> [[a]]
split predicate list =
  let processed_list = skipWhile predicate list
      first_group = takeWhile (not . predicate) processed_list
      remaining = skipWhile (not . predicate) processed_list
   in if null processed_list then [] else first_group : split predicate remaining

startMatches :: String -> String -> Bool
startMatches "" _ = True
startMatches _ "" = False
startMatches (expected : expected_remaining) (input : input_remaining) =
  expected == input && startMatches expected_remaining input_remaining

parseNumber :: String -> Maybe Int
parseNumber input
  | length input > 3 || null input = Nothing
  | not (all isDigit input) = Nothing
  | otherwise = Just $ read input

hoistMaybe :: [Maybe a] -> Maybe [a]
hoistMaybe [] = Just []
hoistMaybe (Just value : rest) = fmap (value :) $ hoistMaybe rest
hoistMaybe (Nothing : _) = Nothing

processSingleInstruction :: String -> Maybe Int
processSingleInstruction instruction =
  if startMatches "mul(" instruction
    then
      let remaining = skip 4 instruction
          body = takeWhile (/= ')') remaining
          items = split (== ',') body
       in if length remaining == length body || length items /= 2
            then Nothing
            else fmap product $ hoistMaybe $ map parseNumber items
    else Nothing

unwrapOr :: a -> Maybe a -> a
unwrapOr _ (Just val) = val
unwrapOr val _ = val

skipUntilDo :: String -> String
skipUntilDo input =
  let processed_input = skipWhile (/= 'd') input
   in if null processed_input
        then ""
        else
          if startMatches "do()" processed_input
            then skip 1 processed_input
            else skipUntilDo $ skip 1 processed_input

process :: String -> Int
process value =
  let processed_value = skipWhile (\val -> val /= 'm' && val /= 'd') value
   in if null processed_value
        then 0
        else
          if startMatches "don't()" processed_value
            then process $ skipUntilDo processed_value
            else
              unwrapOr 0 (processSingleInstruction processed_value) + process (skip 1 processed_value)

main = do
  contents <- getContents
  print $ process contents
