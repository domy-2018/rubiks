

module Cube where

import qualified Data.Map.Strict as M


data Centre = Front | Up | Left | Right | Back | Down
    deriving Show

data Colour = White | Yellow | Red | Orange | Blue | Green
    deriving Show

newtype Cube = Cube { centres :: M.Map Centre Colour }
    deriving Show



