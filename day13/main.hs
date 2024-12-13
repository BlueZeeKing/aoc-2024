import Data.Bifunctor
import Data.List (find, sortBy)
import Data.Maybe
import Debug.Trace

-- Taken from rosettacode, https://rosettacode.org/wiki/Reduced_row_echelon_form#Haskell
rref :: (Fractional a, Eq a) => [[a]] -> [[a]]
rref m = f m 0 [0 .. rows - 1]
  where
    rows = length m
    cols = length $ head m

    f m _ [] = m
    f m lead (r : rs)
      | isNothing indices = m
      | otherwise = f m' (lead' + 1) rs
      where
        indices = find p l
        p (col, row) = m !! row !! col /= 0
        l =
          [ (col, row)
            | col <- [lead .. cols - 1],
              row <- [r .. rows - 1]
          ]

        Just (lead', i) = indices
        newRow = map (/ m !! i !! lead') $ m !! i

        m' =
          zipWith g [0 ..] $
            replace r newRow $
              replace i (m !! r) m
        g n row
          | n == r = row
          | otherwise = zipWith h newRow row
          where
            h = subtract . (* row !! lead')

replace :: Int -> a -> [a] -> [a]
{- Replaces the element at the given index. -}
replace n e l = a ++ e : b
  where
    (a, _ : b) = splitAt n l

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
      matrix = rref $ map (map fromIntegral) [[aX, bX, targetX], [aY, bY, targetY]]
      aPressed = round $ last $ head matrix
      bPressed = round $ last $ last matrix
   in if aPressed * aX + bPressed * bX == targetX && aPressed * aY + bPressed * bY == targetY
        then aPressed * 3 + bPressed
        else 0
findPrice _ = 0

trimStartSpaces :: String -> String
trimStartSpaces = skipWhile (== ' ')

main = do
  contents <- getContents
  print $ sum $ map findPrice $ group 4 $ lines contents
