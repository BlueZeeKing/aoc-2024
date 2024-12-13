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

skip :: Int -> [a] -> [a]
skip _ [] = []
skip 0 rest = rest
skip amountToSkip (_ : rest) = skip (amountToSkip - 1) rest

group :: Int -> [a] -> [[a]]
group _ [] = []
group amount val = take amount val : group amount (skip amount val)

findPrice :: [String] -> Int
findPrice (a : b : target : _) =
  let parse = map (read . (\val -> if head val == '+' then skip 1 val else val) . skip 1 . trimStartSpaces) . split (== ',') . skip 10
      [aX, aY] :: [Int] = parse a
      [bX, bY] :: [Int] = parse b
      [targetX, targetY] :: [Int] = map (read . skip 2 . trimStartSpaces) $ split (== ',') $ skip 7 target
      c2Num = targetY * aX - aY * targetX
      c2Den = bY * aX - bX * aY
      c1Num = targetX * c2Den - c2Num * bX
      c1Den = aX * c2Den
   in if c2Num `mod` c2Den == 0 && c1Num `mod` c1Den == 0
        then c1Num `div` c1Den * 3 + c2Num `div` c2Den
        else 0
findPrice _ = 0

trimStartSpaces :: String -> String
trimStartSpaces = skipWhile (== ' ')

main = do
  contents <- getContents
  print $ sum $ map findPrice $ group 4 $ lines contents
