
import Cube
import Moves
import Control.Monad.RWS.Strict (RWS, runRWS ,get, ask, tell, put)
import Data.Maybe (isJust, fromJust)
import Data.List (foldl')

-- CubeGame is a type synonym to RWS where
-- Reader - [String] is the list of moves to be made
-- Writer - [Cube] list of Cube state transitions for each move
-- State  - Cube current state of the cube
-- result - String is the result after executing the moves
type CubeGame = RWS [Move] [Cube] Cube

-- taking the RWS, executes the moves and presents the result to the user
runGame :: CubeGame String
runGame = do
    userinput <- ask -- [Move]
    cubeState <- get -- Cube
    runMoves userinput cubeState
  where
    runMoves :: [Move] -> Cube -> CubeGame String
    runMoves []     _  = return "done"
    runMoves (m:ms) cs = do
        let newcube = rotateCube m cs
        tell [newcube]
        put newcube
        runMoves ms newcube


-- takes in an input cube, if not starts off with default initCube
-- takes in option to be interactive or batch
-- if batch supply a list of moves, and it will print out result
-- if interactive, it will allow player to interact and keep asking for next move
--
-- <TBC>
-- right now the interactive restarts with initCube all the time. which is wrong (FIXED)
-- the writer resets every time it asks for new move. should keep accumulating until game ends.
--
main :: IO ()
main = runMain initCube
  where
    -- main recursion function
    runMain :: Cube -> IO ()
    runMain c = do
        putStrLn "Make a move: F R B etc.., q to quit"
        input <- getLine
        checkInput c input

    -- does some sanity check on User interactive input. 
    -- can be improved to do some regex to ensure valid Cube moves
    checkInput :: Cube -> String -> IO ()
    checkInput c xs
        | xs == "q"    = putStrLn "Thanks for playing"
        | parseSuccess = startGame c moveList
        | otherwise    = putStrLn "Invalid move detected. Please try again." >> runMain c
      where
        maybeMoveList = map parseMove (words xs)
        parseSuccess  = foldl' (\acc mmove -> acc && isJust mmove) True maybeMoveList
        moveList      = map fromJust maybeMoveList

    -- taking user interactive input, runs the game and executes the moves
    startGame :: Cube -> [Move] -> IO ()
    startGame c moves = do
        let (a,s,_) = runRWS runGame moves c
        print a
        print s
        --print w
        runMain s
