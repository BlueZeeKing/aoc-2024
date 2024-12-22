import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import Debug.Trace (trace)

data DirectionButton = A | Left | Right | Up | Down
  deriving (Ord, Eq, Show)

buttonToCoord :: DirectionButton -> (Int, Int)
buttonToCoord Main.A = (2, 0)
buttonToCoord Main.Up = (1, 0)
buttonToCoord Main.Left = (0, 1)
buttonToCoord Main.Down = (1, 1)
buttonToCoord Main.Right = (2, 1)

keypadToCoord :: Int -> (Int, Int)
keypadToCoord 7 = (0, 0)
keypadToCoord 8 = (1, 0)
keypadToCoord 9 = (2, 0)
keypadToCoord 4 = (0, 1)
keypadToCoord 5 = (1, 1)
keypadToCoord 6 = (2, 1)
keypadToCoord 1 = (0, 2)
keypadToCoord 2 = (1, 2)
keypadToCoord 3 = (2, 2)
keypadToCoord 0 = (1, 3)
keypadToCoord 10 = (2, 3)

pathOnKeypad :: Int -> Int -> [DirectionButton]
pathOnKeypad start target
  | startX == targetX || startY == targetY = movementY ++ movementX
  | startY == 3 && targetX == 0 = movementY ++ movementX
  | targetY == 3 && startX == 0 = movementX ++ movementY
  | targetX < startX = movementX ++ movementY -- I don't know why this works
  | otherwise = movementY ++ movementX
  where
    (startX, startY) = keypadToCoord start
    (targetX, targetY) = keypadToCoord target
    movementY = replicate (abs (startY - targetY)) (if startY < targetY then Main.Down else Main.Up)
    movementX = replicate (abs (startX - targetX)) (if startX < targetX then Main.Right else Main.Left)

pathOnDirection :: DirectionButton -> DirectionButton -> [DirectionButton]
pathOnDirection start target
  | startX == targetX || startY == targetY = movementY ++ movementX
  | startY == 0 && targetX == 0 = movementY ++ movementX
  | targetY == 0 && startX == 0 = movementX ++ movementY
  | targetX < startX = movementX ++ movementY
  | otherwise = movementY ++ movementX
  where
    (startX, startY) = buttonToCoord start
    (targetX, targetY) = buttonToCoord target
    movementY = replicate (abs (startY - targetY)) (if startY < targetY then Main.Down else Main.Up)
    movementX = replicate (abs (startX - targetX)) (if startX < targetX then Main.Right else Main.Left)

findInitialPath :: [Int] -> [DirectionButton]
findInitialPath [val] = []
findInitialPath (fstKey : sndKey : remaining) =
  let currentPath = pathOnKeypad fstKey sndKey
      nextPath = findInitialPath (sndKey : remaining)
   in currentPath ++ [Main.A] ++ nextPath

pairs :: [a] -> [(a, a)]
pairs (fst : snd : remaining) = (fst, snd) : pairs (snd : remaining)
pairs _ = []

numberOfInstructions :: Map.Map (Int, DirectionButton, DirectionButton) Int -> Int -> DirectionButton -> DirectionButton -> (Int, Map.Map (Int, DirectionButton, DirectionButton) Int)
numberOfInstructions cache 0 _ _ = (1, cache)
numberOfInstructions cache depth fstButton sndButton
  | Just result <- Map.lookup (depth, fstButton, sndButton) cache = (result, cache)
  | otherwise =
      let path = Main.A : pathOnDirection fstButton sndButton ++ [Main.A]
          (result, newCache) =
            foldr
              ( \(fstButton, sndButton) (acc, cache) ->
                  let (result, newCache) = numberOfInstructions cache (depth - 1) fstButton sndButton
                   in (acc + result, newCache)
              )
              (0, cache)
              $ pairs path
       in (result, Map.insert (depth, fstButton, sndButton) result newCache)

performRobotPasses :: Int -> [DirectionButton] -> Int
performRobotPasses numPasses path =
  fst
    $ foldr
      ( \(fstButton, sndButton) (acc, cache) ->
          let (result, newCache) = numberOfInstructions cache numPasses fstButton sndButton
           in (acc + result, newCache)
      )
      (0, Map.empty)
    $ pairs (Main.A : path)

main = do
  contents <- getContents
  let codes = lines contents
      numericPart :: [Int] = map (read . init) codes
      parsedCodes = map (map (\value -> if value == 'A' then 10 else read [value])) codes
  vars <- replicateM (length parsedCodes) newEmptyMVar
  mapM_
    ( \(code, numericPart, var) ->
        forkIO
          ( do
              let initialPath = findInitialPath (10 : code)
                  numPresses = performRobotPasses 25 initialPath
              putMVar var (numPresses * numericPart)
          )
    )
    $ zip3 parsedCodes numericPart vars
  result <- mapM takeMVar vars
  print $ sum result
