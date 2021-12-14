{-# LANGUAGE RecordWildCards #-}


import Params
import Cube
import Moves
import Solve
import Control.Monad.RWS.Strict (execRWST, RWST, local, ask, get, put, tell)
import Control.Monad.Trans
import Control.Monad (void)
import Data.Maybe (isJust, fromJust)
import System.Random.Stateful (newStdGen)
import Control.Monad.State (evalState)
import GHC.Base (failIO)

-- CubeGame is a type synonym to RWST where
-- Reader - [Move] is the list of moves to be made
-- Writer - [(Maybe Move, Cube)] list of Cube state transitions for each move, first Move is Nothing
-- State  - Cube current state of the cube
type CubeGame = RWST [Move] [(Maybe Move, Cube)] Cube IO


-- kicks off the batch game
startBatchGame :: CubeGame ()
startBatchGame = do
    curCube <- get
    tell [(Nothing, curCube)]
    runMoves


-- writes the initial cube to writer log, then kicks off the interactive game
startInteractiveGame :: CubeGame ()
startInteractiveGame = do
    liftIO $ putStrLn "**********************************"
    liftIO $ putStrLn "* Welcome to the 2x2 Rubiks Game *"
    liftIO $ putStrLn "**********************************"
    liftIO $ putStrLn "To start a new game, choose your initial starting Cube:"
    liftIO $ putStrLn "    1 - Randomized cube"
    liftIO $ putStrLn "    2 - Interactively enter in initial cube position"
    liftIO $ putStrLn "    3 - Solved cube"
    liftIO $ putStrLn "    4 - Use cube passed in from parameters"
    i <- liftIO getStartingInput
    case i of
        '1' -> liftIO (putStrLn "\nStarting game with randomized cube") >> 
               liftIO newStdGen >>= 
               put . evalState randomCube -- get random generator and pass it to evalState of randomCube to get a Cube
        '2' -> liftIO (putStrLn "") >>
               liftIO enterInteractiveCube >>=
               put
        '3' -> liftIO (putStrLn "\nStarting game with a solved cube") >>
               put initCube 
        _   -> void $ liftIO (putStrLn "\nStarting game with cube from parameters") -- can only be '4' but using _ as catch all
    curCube <- get
    tell [(Nothing, curCube)] -- write the first initial cube to writer
    interactiveGame -- then starts the interactive game

  where
    getStartingInput :: IO Char
    getStartingInput = do
        i <- getChar
        if i `elem` ['1', '2', '3', '4'] then
            return i
        else
            putStrLn "  --> Invalid input. Valid options are 1, 2, 3, 4. Please try again." >> getStartingInput

    enterInteractiveCube :: IO Cube
    enterInteractiveCube = do
        putStrLn "With the cube facing you, enter in the 3 colours of each face of the corners."
        putStrLn "Valid colours are: W Y R O B G"
        let cornerColours = map getCornerColours ["FLU", "FRU", "FLD", "FRD", "BLU", "BRU", "BLD", "BRD"]
        flu_c <- head cornerColours
        fru_c <- cornerColours !! 1
        fld_c <- cornerColours !! 2
        frd_c <- cornerColours !! 3
        blu_c <- cornerColours !! 4
        bru_c <- cornerColours !! 5
        bld_c <- cornerColours !! 6
        brd_c <- cornerColours !! 7
        return Cube { flu = flu_c, fru = fru_c, fld = fld_c, frd = frd_c, blu = blu_c, bru = bru_c, bld = bld_c, brd = brd_c }

    getCornerColours :: String -> IO (Colour, Colour, Colour)
    getCornerColours cs = do
        putStr $ "Enter colours for corner " ++ cs ++ ": "
        input <- getLine
        let pc = parseCorner input
        maybe (putStrLn "Invalid colours entered. Please try again." >> getCornerColours cs) return pc 


-- interactiveGame will recursively prompt the player for input
-- with the input it will then rotate the cube and print the state of the cube
interactiveGame :: CubeGame ()
interactiveGame = do
    curCube <- get
    liftIO $ print curCube
    moves   <- liftIO getInteractiveInput
    case moves of
        Left 'q' -> liftIO $ putStrLn "Thank you for playing"
        Left 's' -> local (const $ solveCube curCube) runMoves >> -- TBC needs to tidy up the user feedback here
                    interactiveGame
        Right ms -> local (const ms) runMoves >>
                    interactiveGame
        Left _   -> liftIO $ failIO "Something has gone terribly wrong." -- should never reach here
  where
    -- get the input from player
    getInteractiveInput :: IO (Either Char [Move])
    getInteractiveInput = do
        putStrLn "Make a move: F R U B L D, s to auto solve, q to quit"
        input <- parseInput <$> getLine
        case input of
            Right [] -> putStrLn "Invalid move. Please try again" >> getInteractiveInput
            _        -> return input

    -- parses String to a Either String [Move]. If "q" or "s" return Left, if invalid move, return empty list
    parseInput :: String -> Either Char [Move]
    parseInput i
        | i == "q"             = Left 'q'
        | i == "s"             = Left 's'
        | isJust maybeMoveList = Right $ fromJust maybeMoveList
        | otherwise            = Right []
      where
        maybeMoveList = parseMoves i

-- execute the [Move] in Reader
-- writes to Writer which is [Cube]
-- updates State which is Cube
runMoves :: CubeGame ()
runMoves = do
    moveList  <- ask
    cubeState <- get
    execMoves moveList cubeState
  where
    execMoves :: [Move] -> Cube -> CubeGame ()
    execMoves [] _      = return ()
    execMoves (m:ms) cs = do
        let newcube = rotateCube m cs
        tell [(Just m, newcube)]
        put newcube
        execMoves ms newcube

-- prettyWriter turns writer into a nice log
prettyWriter :: [(Maybe Move, Cube)] -> String
prettyWriter []                = []
prettyWriter ((Nothing, c):xs) = "Start game:\n" ++ show c ++ prettyWriter xs
prettyWriter ((Just m, c):xs)  = "Move " ++ show m ++ "\n" ++ show c ++ prettyWriter xs

-- main to handle option parameters:
--  - interactive (batch mode?)
--  - player to pass in initial cube state, or by default a randomized cube, option to start with solved cube
--  - option to log to file, pass in filename if selected
main :: IO ()
main = do
    Params {..} <- cmdLineParser
    --print mode
    --print cubeParams
    --print moveParams
    --print logFile
    (_, w) <- case mode of
                Batch       -> execRWST startBatchGame moveParams cubeParams
                Interactive -> execRWST startInteractiveGame [] cubeParams
    writeFile logFile (prettyWriter w)
    --print s
    --print w







