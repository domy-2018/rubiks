

module Cube where


import           Prelude         hiding (Left, Right)
--import qualified Data.Map.Strict as M
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Moves


data Colour = White | Yellow | Red | Orange | Blue | Green
    deriving Show

-- A 2x2 Rubiks cube consists of 8 corners.
-- This is represented by an IntMap from 0-7 with a triple of colours
-- 0 -> Front Left  Up
-- 1 -> Front Right Up
-- 2 -> Front Left  Down
-- 3 -> Front Right Down
-- 4 -> Back  Left  Up
-- 5 -> Back  Right Up
-- 6 -> Back  Left  Down
-- 7 -> Back  Right Down
newtype Cube = Cube { corners :: IntMap (Colour, Colour, Colour) }
    deriving Show

-- initialize the cube
initCube :: Cube
initCube = Cube {
               corners = IntMap.fromList [(0, (White,  Blue,  Red)), 
                                          (1, (White,  Green, Red)),
                                          (2, (White,  Blue,  Orange)),
                                          (3, (White,  Green, Orange)),
                                          (4, (Yellow, Blue,  Red)),
                                          (5, (Yellow, Green, Red)),
                                          (6, (Yellow, Blue,  Orange)),
                                          (7, (Yellow, Green, Orange))]
           }

-- rotates Cube given move
rotateCube :: Move -> Cube -> Cube
rotateCube F c = Cube $ 
    IntMap.adjust (const (c1F, c1U, c1R)) 3 $
    IntMap.adjust (const (c3F, c3D, c3R)) 2 $
    IntMap.adjust (const (c0F, c0U, c0L)) 1 $
    IntMap.adjust (const (c2F, c2D, c2L)) 0 cornerMap
  where
    cornerMap :: IntMap (Colour, Colour, Colour)
    cornerMap = corners c

    (c0F, c0L, c0U) = cornerMap IntMap.! 0
    (c1F, c1R, c1U) = cornerMap IntMap.! 1
    (c2F, c2L, c2D) = cornerMap IntMap.! 2
    (c3F, c3R, c3D) = cornerMap IntMap.! 3


rotateCube _ _ = undefined
