import Data.Bits
import Debug.Trace (trace)

mix :: Int -> Int -> Int
mix = xor

prune :: Int -> Int
prune secret = secret .&. (2 ^ 24 - 1)

evolveSecretNumber :: Int -> Int
evolveSecretNumber secret =
  let stage1 = prune $ mix (secret * 64) secret
      stage2 = prune $ mix (stage1 `shiftR` 5) stage1
   in prune $ mix (stage2 `shiftL` 11) stage2

main = do
  contents <- getContents
  print $ sum $ map (\secret -> foldr (\_ acc -> evolveSecretNumber acc) (read secret) [1 .. 2000]) $ lines contents
