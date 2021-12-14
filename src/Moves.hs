{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Moves where

import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import System.Random.Stateful
import GHC.Generics (Generic)
import Control.Monad.State (State, state)
import Control.Monad (replicateM)

data Move = F  | U  | L  | R  | B  | D  |
            F' | U' | L' | R' | B' | D' |
            F2 | U2 | L2 | R2 | B2 | D2
    deriving (Show, Read, Eq, Generic, Uniform)

-- using State to generate a random Move
randomMove :: State StdGen Move
randomMove = state uniform

-- generate multiple random Moves
randomMoveList :: Int -> State StdGen [Move]
randomMoveList n = replicateM n randomMove

-- parses String to Move
parseMove :: String -> Maybe Move
parseMove = readMaybe

{-parseMove m
    | m == "F"  = Just F
    | m == "U"  = Just U
    | m == "L"  = Just L
    | m == "R"  = Just R
    | m == "B"  = Just B
    | m == "D"  = Just D
    | m == "F'" = Just F'
    | m == "U'" = Just U'
    | m == "L'" = Just L'
    | m == "R'" = Just R'
    | m == "B'" = Just B'
    | m == "D'" = Just D'
    | m == "F2" = Just F2
    | m == "U2" = Just U2
    | m == "L2" = Just L2
    | m == "R2" = Just R2
    | m == "B2" = Just B2
    | m == "D2" = Just D2
    | otherwise = Nothing
-}
-- parses String to Maybe list of Move
parseMoves :: String -> Maybe [Move]
parseMoves ms
    | Nothing `elem` maybeMoveList = Nothing
    | otherwise                    = Just $ map fromJust maybeMoveList
  where
    maybeMoveList = map parseMove (words ms)


