import qualified Data.Map as Map
import Data.Maybe (catMaybes)

data DirectionButton = A | Left | Right | Up | Down
  deriving (Ord, Eq, Show)

data State = State
  { keypad :: Int,
    fstDir :: DirectionButton,
    sndDir :: DirectionButton
  }
  deriving (Ord, Eq, Show)

getSurroundingDirections :: DirectionButton -> [DirectionButton]
getSurroundingDirections Main.A = [Main.Up, Main.Right]
getSurroundingDirections Main.Left = [Main.Up, Main.Down]
getSurroundingDirections Main.Right = [Main.A, Main.Down]
getSurroundingDirections Main.Up = [Main.A, Main.Down]
getSurroundingDirections Main.Down = [Main.Up, Main.Left, Main.Right]

-- up down left right
keypadDirectionMap :: Int -> (Maybe Int, Maybe Int, Maybe Int, Maybe Int)
keypadDirectionMap 0 = (Just 2, Nothing, Nothing, Just 10)
keypadDirectionMap 1 = (Just 4, Nothing, Nothing, Just 2)
keypadDirectionMap 2 = (Just 5, Just 0, Just 1, Just 3)
keypadDirectionMap 3 = (Just 6, Just 10, Just 2, Nothing)
keypadDirectionMap 4 = (Just 7, Just 1, Nothing, Just 5)
keypadDirectionMap 5 = (Just 8, Just 2, Just 4, Just 6)
keypadDirectionMap 6 = (Just 9, Just 3, Just 5, Nothing)
keypadDirectionMap 7 = (Nothing, Just 4, Nothing, Just 8)
keypadDirectionMap 8 = (Nothing, Just 5, Just 7, Just 9)
keypadDirectionMap 9 = (Nothing, Just 6, Just 8, Nothing)
keypadDirectionMap 10 = (Just 3, Nothing, Just 0, Nothing)

getKeypadFromMap :: (Maybe Int, Maybe Int, Maybe Int, Maybe Int) -> DirectionButton -> Maybe Int
getKeypadFromMap (up, down, left, right) Main.Up = up
getKeypadFromMap (up, down, left, right) Main.Down = down
getKeypadFromMap (up, down, left, right) Main.Left = left
getKeypadFromMap (up, down, left, right) Main.Right = right

-- up down left right
directionMap :: DirectionButton -> (Maybe DirectionButton, Maybe DirectionButton, Maybe DirectionButton, Maybe DirectionButton)
directionMap Main.A = (Nothing, Just Main.Right, Just Main.Up, Nothing)
directionMap Main.Up = (Nothing, Just Main.Down, Nothing, Just Main.A)
directionMap Main.Down = (Just Main.Up, Nothing, Just Main.Left, Just Main.Right)
directionMap Main.Left = (Nothing, Nothing, Nothing, Just Main.Down)
directionMap Main.Right = (Just Main.A, Nothing, Just Main.Down, Nothing)

getDirectionFromMap :: (Maybe DirectionButton, Maybe DirectionButton, Maybe DirectionButton, Maybe DirectionButton) -> DirectionButton -> Maybe DirectionButton
getDirectionFromMap (up, down, left, right) Main.Up = up
getDirectionFromMap (up, down, left, right) Main.Down = down
getDirectionFromMap (up, down, left, right) Main.Left = left
getDirectionFromMap (up, down, left, right) Main.Right = right

pressA :: State -> Maybe State
pressA State {keypad, fstDir, sndDir}
  | fstDir == Main.A && sndDir == Main.A = Nothing
  | fstDir == Main.A = case getKeypadFromMap (keypadDirectionMap keypad) sndDir of
      Just val -> Just State {keypad = val, fstDir = fstDir, sndDir = sndDir}
      Nothing -> Nothing
  | otherwise = case getDirectionFromMap (directionMap sndDir) fstDir of
      Just val -> Just State {keypad = keypad, fstDir = fstDir, sndDir = val}
      Nothing -> Nothing

getSurroundingStates :: State -> [State]
getSurroundingStates State {keypad, fstDir, sndDir} =
  catMaybes $
    pressA State {keypad = keypad, fstDir = fstDir, sndDir = sndDir}
      : map
        ( fmap
            ( \newFstDir ->
                State {keypad = keypad, fstDir = newFstDir, sndDir = sndDir}
            )
            . getDirectionFromMap (directionMap fstDir)
        )
        [Main.Up, Main.Down, Main.Left, Main.Right]

bfs :: Map.Map State Int -> State -> [State] -> Int
bfs visited target (nextState : remaining)
  | target == nextState = visited Map.! target
  | otherwise =
      let surrounding = filter (`Map.notMember` visited) $ getSurroundingStates nextState
          currentCost = visited Map.! nextState
       in bfs (foldr (`Map.insert` (currentCost + 1)) visited surrounding) target (remaining ++ surrounding)

findLength :: [State] -> Int
findLength (initial : target : remaining) = bfs (Map.insert initial 0 Map.empty) target [initial] + 1 + findLength (target : remaining)
findLength _ = 0

main = do
  contents <- getContents
  let codes = lines contents
      numericPart :: [Int] = map (read . init) codes
      initialState = State {keypad = 10, fstDir = Main.A, sndDir = Main.A}
      complexity = zipWith (*) numericPart $ map (findLength . (initialState :) . map (\value -> State {keypad = if value == 'A' then 10 else read [value], fstDir = Main.A, sndDir = Main.A})) codes
  print $ sum complexity
