
module Cube where

import qualified Moves     as M
import           Text.Read      (readMaybe)
import           Data.List      (foldl', nub)
import           System.Random.Stateful
import           Control.Monad.State (State)

-- W = White
-- Y = Yellow
-- R = Red
-- O = Orange
-- B = Blue
-- G = Green
data Colour = W | Y | R | O | B | G
    deriving (Show, Read, Eq)

-- A 2x2 Rubiks cube consists of 8 corners.
-- This is represented by a record syntax of 8 triples of colours
-- flu -> Front Left  Up
-- fru -> Front Right Up
-- fld -> Front Left  Down
-- frd -> Front Right Down
-- blu -> Back  Left  Up
-- bru -> Back  Right Up
-- bld -> Back  Left  Down
-- brd -> Back  Right Down
data Cube = Cube { flu :: (Colour, Colour, Colour)
                 , fru :: (Colour, Colour, Colour)
                 , fld :: (Colour, Colour, Colour)
                 , frd :: (Colour, Colour, Colour)
                 , blu :: (Colour, Colour, Colour)
                 , bru :: (Colour, Colour, Colour)
                 , bld :: (Colour, Colour, Colour)
                 , brd :: (Colour, Colour, Colour) }
    deriving (Eq, Read)


instance Show Cube where
    show Cube {flu = (f_flu, l_flu, u_flu), 
               fru = (f_fru, r_fru, u_fru),
               fld = (f_fld, l_fld, d_fld),
               frd = (f_frd, r_frd, d_frd),
               blu = (b_blu, l_blu, u_blu),
               bru = (b_bru, r_bru, u_bru),
               bld = (b_bld, l_bld, d_bld),
               brd = (b_brd, r_brd, d_brd)} = "    |" ++ show u_blu ++ "|" ++ show u_bru ++ "|\n" ++
                                              "    |" ++ show u_flu ++ "|" ++ show u_fru ++ "|\n" ++
        "|" ++ show l_blu ++ "|" ++ show l_flu ++ "|" ++ show f_flu ++ "|" ++ show f_fru ++ "|" ++ show r_fru ++ "|" ++ show r_bru ++ "|" ++ show b_bru ++ "|" ++ show b_blu ++ "|\n" ++
        "|" ++ show l_bld ++ "|" ++ show l_fld ++ "|" ++ show f_fld ++ "|" ++ show f_frd ++ "|" ++ show r_frd ++ "|" ++ show r_brd ++ "|" ++ show b_brd ++ "|" ++ show b_bld ++ "|\n" ++
                                              "    |" ++ show d_fld ++ "|" ++ show d_frd ++ "|\n" ++
                                              "    |" ++ show d_bld ++ "|" ++ show d_brd ++ "|\n" 



-- parse Colour
parseColour :: String -> Maybe Colour
parseColour = readMaybe


-- give the opposite colour
oppositeColour :: Colour -> Colour
oppositeColour c = case c of
                       W -> Y
                       Y -> W
                       B -> G
                       G -> B
                       R -> O
                       O -> R

-- parse corner
parseCorner :: String -> Maybe (Colour, Colour, Colour)
parseCorner c = case map parseColour (words c) of
                    [Just c1, Just c2, Just c3] -> Just (c1, c2, c3)
                    _                           -> Nothing

-- initialize the cube
initCube :: Cube
initCube = Cube { flu = (W, B, R)
                , fru = (W, G, R)
                , fld = (W, B, O)
                , frd = (W, G, O)
                , blu = (Y, B, R)
                , bru = (Y, G, R)
                , bld = (Y, B, O)
                , brd = (Y, G, O) }

-- create a random cube by applying 50 random Moves on a solved Cube
randomCube :: State StdGen Cube
randomCube = do
    movelist <- M.randomMoveList 50
    return $ rotateMovesCube movelist initCube


rotateMovesCube :: [M.Move] -> Cube -> Cube
rotateMovesCube ms c = foldl' (flip rotateCube) c ms

-- rotates Cube given move
rotateCube :: M.Move -> Cube -> Cube
rotateCube M.F c@Cube { flu = (f_flu, l_flu, u_flu)
                      , fru = (f_fru, r_fru, u_fru)
                      , fld = (f_fld, l_fld, d_fld)
                      , frd = (f_frd, r_frd, d_frd) } = c { flu = (f_fld, d_fld, l_fld)
                                                          , fru = (f_flu, u_flu, l_flu)
                                                          , fld = (f_frd, d_frd, r_frd)
                                                          , frd = (f_fru, u_fru, r_fru) }

rotateCube M.U c@Cube { flu = (f_flu, l_flu, u_flu)
                      , fru = (f_fru, r_fru, u_fru)
                      , blu = (b_blu, l_blu, u_blu)
                      , bru = (b_bru, r_bru, u_bru) } = c { flu = (r_fru, f_fru, u_fru)
                                                          , fru = (r_bru, b_bru, u_bru)
                                                          , blu = (l_flu, f_flu, u_flu)
                                                          , bru = (l_blu, b_blu, u_blu) }

rotateCube M.L c@Cube { flu = (f_flu, l_flu, u_flu)
                      , fld = (f_fld, l_fld, d_fld)
                      , blu = (b_blu, l_blu, u_blu)
                      , bld = (b_bld, l_bld, d_bld) } = c { flu = (u_blu, l_blu, b_blu)
                                                          , fld = (u_flu, l_flu, f_flu)
                                                          , blu = (d_bld, l_bld, b_bld)
                                                          , bld = (d_fld, l_fld, f_fld) }

rotateCube M.R c@Cube { fru = (f_fru, r_fru, u_fru)
                      , frd = (f_frd, r_frd, d_frd)
                      , bru = (b_bru, r_bru, u_bru)
                      , brd = (b_brd, r_brd, d_brd) } = c { fru = (d_frd, r_frd, f_frd)
                                                          , frd = (d_brd, r_brd, b_brd)
                                                          , bru = (u_fru, r_fru, f_fru)
                                                          , brd = (u_bru, r_bru, b_bru) }

rotateCube M.B c@Cube { blu = (b_blu, l_blu, u_blu)
                      , bru = (b_bru, r_bru, u_bru)
                      , bld = (b_bld, l_bld, d_bld)
                      , brd = (b_brd, r_brd, d_brd) } = c { blu = (b_bru, u_bru, r_bru)
                                                          , bru = (b_brd, d_brd, r_brd)
                                                          , bld = (b_blu, u_blu, l_blu)
                                                          , brd = (b_bld, d_bld, l_bld) }

rotateCube M.D c@Cube { fld = (f_fld, l_fld, d_fld)
                      , frd = (f_frd, r_frd, d_frd)
                      , bld = (b_bld, l_bld, d_bld)
                      , brd = (b_brd, r_brd, d_brd) } = c { fld = (l_bld, b_bld, d_bld)
                                                          , frd = (l_fld, f_fld, d_fld)
                                                          , bld = (r_brd, b_brd, d_brd)
                                                          , brd = (r_frd, f_frd, d_frd) }

rotateCube M.F' c = rotateCube M.F $ rotateCube M.F $ rotateCube M.F c

rotateCube M.U' c = rotateCube M.U $ rotateCube M.U $ rotateCube M.U c

rotateCube M.L' c = rotateCube M.L $ rotateCube M.L $ rotateCube M.L c

rotateCube M.R' c = rotateCube M.R $ rotateCube M.R $ rotateCube M.R c

rotateCube M.B' c = rotateCube M.B $ rotateCube M.B $ rotateCube M.B c

rotateCube M.D' c = rotateCube M.D $ rotateCube M.D $ rotateCube M.D c

rotateCube M.F2 c = rotateCube M.F $ rotateCube M.F c

rotateCube M.U2 c = rotateCube M.U $ rotateCube M.U c

rotateCube M.L2 c = rotateCube M.L $ rotateCube M.L c

rotateCube M.R2 c = rotateCube M.R $ rotateCube M.R c

rotateCube M.B2 c = rotateCube M.B $ rotateCube M.B c

rotateCube M.D2 c = rotateCube M.D $ rotateCube M.D c


-- given a cube, checks to see if it is in a solved state or not
-- cube is solved if all the faces have the same colour, and each face has a different colour
isItSolved :: Cube -> Bool
isItSolved Cube {flu = (f_flu, l_flu, u_flu),
                 fru = (f_fru, r_fru, u_fru),
                 fld = (f_fld, l_fld, d_fld),
                 frd = (f_frd, r_frd, d_frd),
                 blu = (b_blu, l_blu, u_blu),
                 bru = (b_bru, r_bru, u_bru),
                 bld = (b_bld, l_bld, d_bld),
                 brd = (b_brd, r_brd, d_brd)} = [f_fru, f_fld, f_frd] == [f_flu, f_flu, f_flu] &&
                                                [b_bru, b_bld, b_brd] == [b_blu, b_blu, b_blu] &&
                                                [l_flu, l_fld, l_blu] == [l_bld, l_bld, l_bld] &&
                                                [r_fru, r_frd, r_bru] == [r_brd, r_brd, r_brd] &&
                                                [u_flu, u_fru, u_blu] == [u_bru, u_bru, u_bru] &&
                                                [d_fld, d_frd, d_bld] == [d_brd, d_brd, d_brd] &&
                                                nub [f_flu, b_blu, l_bld, r_brd, u_bru, d_brd] == [f_flu, b_blu, l_bld, r_brd, u_bru, d_brd]



