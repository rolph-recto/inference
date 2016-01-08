-- River.hs
-- implementation of the river crossing puzzle
-- inspired by http://blog.jle.im/entry/wolf-goat-cabbage-the-list-monadplus-logic-problems
-- although this implementation is markedly more "monadic" in flavor,
-- as I employ a monad stack with a writer to explicitly keep track
-- of path (move) information

import Control.Monad
import Control.Monad.Writer
import Control.Monad.State

-- left or right
data Pos = L | R deriving (Eq, Show)

-- what does the farmer bring with him when he crosses the river?
data Item = Wolf | Sheep | Cabbage | None deriving (Eq, Show)
data Move = Move Pos Item deriving (Eq)

instance Show Move where
  show (Move pos item) = show pos ++ " " ++ show item

-- wolf, sheep, cabbage, farmer
-- wolf can't be alone with sheep
-- sheep can't be alone with cabbage
data PuzzleState = PuzzleState Pos Pos Pos Pos deriving (Show)

type PuzzleExec a = WriterT [Move] (StateT PuzzleState []) a

isGoal :: PuzzleState -> Bool
isGoal state
  | PuzzleState R R R R <- state = True
  | otherwise                    = False

validState :: PuzzleState -> Bool
validState (PuzzleState wolf sheep cabbage farmer)
  | wolf == sheep && wolf /= farmer     = False
  | sheep == cabbage && sheep /= farmer = False
  | otherwise                           = True

possibleMoves :: PuzzleState -> [Move]
possibleMoves state@(PuzzleState wolf sheep cabbage farmer) =
  filter (validState . applyMove state) moves
  where passengers      = [Wolf, Sheep, Cabbage, None]
        inLocation      = [wolf == farmer, sheep == farmer, cabbage == farmer, True]
        -- only possible passengers are those in the same location as farmer
        validPassengers = map snd $ filter fst $ zip inLocation passengers
        moves           = case farmer of
                            L -> map (Move R) validPassengers
                            R -> map (Move L) validPassengers 

applyMove :: PuzzleState -> Move -> PuzzleState
applyMove (PuzzleState wolf sheep cabbage farmer) (Move dir item)
  | item == Wolf    = PuzzleState dir sheep cabbage dir
  | item == Sheep   = PuzzleState wolf dir cabbage dir
  | item == Cabbage = PuzzleState wolf sheep dir dir
  | item == None    = PuzzleState wolf sheep cabbage dir

step :: PuzzleExec ()
step = do
  pstate <- lift get
  -- if we are in the goal state already, don't move
  if isGoal pstate
  then return ()
  else do 
    -- execute all possible moves (we "fork" here)
    -- this seems a little hacky; perhaps we can write the following
    -- line more monadically (i.e., in do-syntax)?
    move <- lift $ StateT executeMove
    tell [move]

  where executeMove s = [(m, applyMove s m) | m <- possibleMoves s]

-- returns all possible winning paths with at most n steps
solvePuzzle :: Int -> [[Move]]
solvePuzzle n = evalStateT (execWriterT winPaths) initState
  where stepN     = foldr1 (>>) (replicate n step)
        winPaths  = stepN >> lift get >>= (guard . isGoal)
        initState = PuzzleState L L L L

main = do
  -- output all possible winning paths in 10 moves or less
  let solutions = solvePuzzle 10
  forM solutions print
