-- This algorithm is based on:  https://github.com/KitN/rubiks-two-two
module BruteForceAlgorithm where

import Cube
import Moves
import Data.Digits (digits)



-- given a cube, return a list of moves that will solve it
-- using digits convert base 10 to base 6
-- then map each base 6 number to a Move to get a list of moves
-- then apply it and check if is solved.
-- if solved, return list of moves. If not, recurse
bruteForceSolve :: Cube -> [Move]
bruteForceSolve c
    | isItSolved c = []
    | otherwise    = runBruteForce bruteForceMoves c
  where
    runBruteForce [] _ = []
    runBruteForce (x:xs) cb
        | isItSolved $ rotateMovesCube x c = x
        | otherwise                        = runBruteForce xs cb

-- NOTE: this algorithm only works on cubes that are almost solved.
--       on fully randomized cubes, it will not complete, run out of memory and crash
bruteForceMoves :: [[Move]]
bruteForceMoves = [toEnum 0] : map (map toEnum . digits 6) [1..78364164095]





