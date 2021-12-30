{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Moves where

import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import System.Random.Stateful
import GHC.Generics (Generic)
import Control.Monad.State (State, state)
import Control.Monad (replicateM)

-- list of all acceptable moves
data Move = F  | U  | R  | F' | U' | R' |
            B  | L  | D  | B' | L' | D' |
            F2 | U2 | R2 | B2 | L2 | D2
    deriving (Show, Read, Eq, Generic, Uniform, Enum)

-- using State to generate a random Move
randomMove :: State StdGen Move
randomMove = state uniform

-- generate multiple random Moves
randomMoveList :: Int -> State StdGen [Move]
randomMoveList n = replicateM n randomMove

-- parses String to Move
parseMove :: String -> Maybe Move
parseMove = readMaybe

-- parses String to Maybe list of Move
parseMoves :: String -> Maybe [Move]
parseMoves ms
    | Nothing `elem` maybeMoveList = Nothing
    | otherwise                    = Just $ map fromJust maybeMoveList
  where
    maybeMoveList = map parseMove (words ms)


