
module Solve where

import Cube
import Moves
import Control.Monad.RWS.Strict

-- Solver is a type synonym for RWS monad 
-- Reader - Cube that is to be solved
-- Writer - list of moves to solve
-- State  - final state of cube
type Solver = RWS Cube [Move] Cube ()


solveCube :: Cube -> [Move]
solveCube = undefined




