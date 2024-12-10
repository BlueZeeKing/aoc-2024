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

flatten :: [[a]] -> [a]
flatten [[]] = []
flatten ([] : (next : innerRemaining) : remaining) = next : flatten (innerRemaining : remaining)
flatten ((next : innerRemaining) : remaining) = next : flatten (innerRemaining : remaining)

fancyMatches :: String -> [String] -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> Bool
fancyMatches "" _ _ _ _ _ = True
fancyMatches _ input n _ _ _
  | n <= -1 || n >= length input = False
fancyMatches _ input _ n _ _
  | n <= -1 || n >= length (head input) = False
fancyMatches (firstExpected : remainingExpected) input first second modifyFirst modifySecond =
  firstExpected == input !! first !! second && fancyMatches remainingExpected input (modifyFirst first) (modifySecond second) modifyFirst modifySecond

xMatches :: [String] -> Int -> Int -> Bool
xMatches input first second =
  let partialFancyMatches = fancyMatches "MAS" input
      possibleMatches =
        [ partialFancyMatches firstIdx secondIdx firstModifier secondModifier
          | (firstModifier, firstIdx) <- [((+ 1), first), ((+ (-1)), first + 2)],
            (secondModifier, secondIdx) <- [((+ 1), second), ((+ (-1)), second + 2)]
        ]
   in length (filter id possibleMatches) == 2

main = do
  contents <- getContents
  let field = split (== '\n') contents
      startPoints = flatten $ map (\val -> map (val,) [0 .. length (head field) - 1]) [0 .. length field - 1]
      matches = filter id $ map (uncurry (xMatches field)) startPoints
   in print $ length matches
