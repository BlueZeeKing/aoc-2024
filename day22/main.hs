import Data.Bits
import qualified Data.Map as Map

mix :: Int -> Int -> Int
mix = xor

prune :: Int -> Int
prune secret = secret .&. (2 ^ 24 - 1)

evolveSecretNumber :: Int -> Int
evolveSecretNumber secret =
  let stage1 = prune $ mix (secret * 64) secret
      stage2 = prune $ mix (stage1 `shiftR` 5) stage1
   in prune $ mix (stage2 `shiftL` 11) stage2

pairs :: [a] -> [(a, a)]
pairs (fst : snd : remaining) = (fst, snd) : pairs (snd : remaining)
pairs _ = []

group :: Int -> [a] -> [[a]]
group amount (fst : remaining)
  | length remaining < amount - 1 = []
  | otherwise = (fst : take (amount - 1) remaining) : group amount remaining

skip :: Int -> [a] -> [a]
skip _ [] = []
skip 0 rest = rest
skip amountToSkip (_ : rest) = skip (amountToSkip - 1) rest

main = do
  contents <- getContents
  let validPair a b = a + b <= 9 && a + b >= -9
      prices =
        map
          ( \secret ->
              map (`mod` 10) $ scanl (\acc _ -> evolveSecretNumber acc) (read secret) [1 .. 2000]
          )
          $ lines contents
      changes = map (\price -> zip (tail price) (map (\(a, b) -> b - a) $ pairs price)) prices
      allPossibleSequences =
        foldr
          ( \(key, value) acc ->
              if Map.member key acc
                then Map.adjust (+ value) key acc
                else Map.insert key value acc
          )
          Map.empty
          $ foldr1 (++)
          $ map
            ( (Map.toList . Map.fromList . reverse)
                . (\change -> zip (group 4 $ map snd change) (skip 3 $ map fst change))
            )
            changes
  print $ maximum $ map snd $ Map.toList allPossibleSequences
