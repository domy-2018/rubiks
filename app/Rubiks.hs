
import Cube
import Control.Monad.RWS.Strict

-- CubeGame is a type synonym to RWS where
-- Reader - [String] is the list of moves to be made
-- Writer - [Cube] list of Cube state transitions for each move
-- State  - Cube current state of the cube
-- result - String is the result after executing the moves
type CubeGame = RWS [String] [Cube] Cube

-- taking the RWS, executes the moves and presents the result to the user
startGame :: CubeGame String
startGame = do
    userinput <- ask
    return $ "Performing the following moves: " ++ show userinput


main :: IO ()
main = runMain
  where
    -- main recursion function
    runMain = do
        putStrLn "Make a move: F R B etc.., q to quit"
        input <- getLine
        checkInput input

    -- does some sanity check on User interactive input. 
    -- can be improved to do some regex to ensure valid Cube moves
    checkInput :: String -> IO ()
    checkInput a
        | a /= "q"  = runGame a
        | otherwise = putStrLn "Thanks for playing"

    -- taking user interactive input, runs the game and executes the moves
    runGame :: String -> IO ()  
    runGame input = do
        let inputList = words input 
            (a,s,w)   = runRWS startGame inputList initCube
        print a
        print s
        runMain
