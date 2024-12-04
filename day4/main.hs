skipWhile :: (a -> Bool) -> [a] -> [a]
skipWhile _ [] = []
skipWhile predicate (start : rest) =
  if predicate start
    then skipWhile predicate rest
    else start : rest

split :: (a -> Bool) -> [a] -> [[a]]
split predicate list =
  let processed_list = skipWhile predicate list
      first_group = takeWhile (not . predicate) processed_list
      remaining = skipWhile (not . predicate) processed_list
   in if null processed_list then [] else first_group : split predicate remaining

flatten :: [[a]] -> [a]
flatten [[]] = []
flatten ([] : (next : inner_remaining) : remaining) = next : flatten (inner_remaining : remaining)
flatten ((next : inner_remaining) : remaining) = next : flatten (inner_remaining : remaining)

fancyMatches :: String -> [String] -> Int -> Int -> (Int -> Int) -> (Int -> Int) -> Bool
fancyMatches "" _ _ _ _ _ = True
fancyMatches _ input n _ _ _
  | n == -1 || n == length input = False
fancyMatches _ input _ n _ _
  | n == -1 || n == length (head input) = False
fancyMatches (firstExpected : remainingExpected) input first second modifyFirst modifySecond =
  firstExpected == input !! first !! second && fancyMatches remainingExpected input (modifyFirst first) (modifySecond second) modifyFirst modifySecond

countAtPoint :: [String] -> Int -> Int -> Int
countAtPoint input first second =
  let partialFancyMatches = fancyMatches "XMAS" input first second
   in length $
        filter
          id
          [ partialFancyMatches first second
            | first <- [(+ 1), id, (+ (-1))],
              second <- [(+ 1), id, (+ (-1))],
              not (first 1 == 1 && second 1 == 1)
          ]

main = do
  contents <- getContents
  let field = split (== '\n') contents
      startPoints = flatten $ map (\val -> map (val,) [0 .. length (head field) - 1]) [0 .. length field - 1]
      matches = map (uncurry (countAtPoint field)) startPoints
   in print $ sum matches
