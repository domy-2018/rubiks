

module Cube where


import           Prelude         hiding (Left, Right)
import           Moves


data Colour = White | Yellow | Red | Orange | Blue | Green

instance Show Colour where
    show White  = "W"
    show Yellow = "Y"
    show Red    = "R"
    show Orange = "O"
    show Blue   = "B"
    show Green  = "G"

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
    deriving Show

-- initialize the cube
initCube :: Cube
initCube = Cube { flu = (White,  Blue,  Red)
                , fru = (White,  Green, Red)
                , fld = (White,  Blue,  Orange)
                , frd = (White,  Green, Orange)
                , blu = (Yellow, Blue,  Red)
                , bru = (Yellow, Green, Red)
                , bld = (Yellow, Blue,  Orange)
                , brd = (Yellow, Green, Orange) }

-- rotates Cube given move
rotateCube :: Move -> Cube -> Cube
rotateCube F c@Cube { flu = (f_flu, l_flu, u_flu)
                    , fru = (f_fru, r_fru, u_fru)
                    , fld = (f_fld, l_fld, d_fld)
                    , frd = (f_frd, r_frd, d_frd) } = c { flu = (f_fld, d_fld, l_fld)
                                                        , fru = (f_flu, u_flu, l_flu)
                                                        , fld = (f_frd, d_frd, r_frd)
                                                        , frd = (f_fru, u_fru, r_fru) }

rotateCube U c@Cube { flu = (f_flu, l_flu, u_flu)
                    , fru = (f_fru, r_fru, u_fru)
                    , blu = (b_blu, l_blu, u_blu)
                    , bru = (b_bru, r_bru, u_bru) } = c { flu = (r_fru, f_fru, u_fru)
                                                        , fru = (r_bru, b_bru, u_bru)
                                                        , blu = (l_flu, f_flu, u_flu)
                                                        , bru = (l_blu, b_blu, u_blu) }

rotateCube L c@Cube { flu = (f_flu, l_flu, u_flu)
                    , fld = (f_fld, l_fld, d_fld)
                    , blu = (b_blu, l_blu, u_blu)
                    , bld = (b_bld, l_bld, d_bld) } = c { flu = (u_blu, l_blu, b_blu)
                                                        , fld = (u_flu, l_flu, f_flu)
                                                        , blu = (d_bld, l_bld, b_bld)
                                                        , bld = (d_fld, l_fld, f_fld) }

rotateCube R c@Cube { fru = (f_fru, r_fru, u_fru)
                    , frd = (f_frd, r_frd, d_frd)
                    , bru = (b_bru, r_bru, u_bru)
                    , brd = (b_brd, r_brd, d_brd) } = c { fru = (d_frd, r_frd, f_frd)
                                                        , frd = (d_brd, r_brd, b_brd)
                                                        , bru = (u_fru, r_fru, f_fru)
                                                        , brd = (u_bru, r_bru, b_bru) }

rotateCube B c@Cube { blu = (b_blu, l_blu, u_blu)
                    , bru = (b_bru, r_bru, u_bru)
                    , bld = (b_bld, l_bld, d_bld)
                    , brd = (b_brd, r_brd, d_brd) } = c { blu = (b_bru, u_bru, r_bru)
                                                        , bru = (b_brd, d_brd, r_brd)
                                                        , bld = (b_blu, u_blu, l_blu)
                                                        , brd = (b_bld, d_bld, l_bld) }

rotateCube D c@Cube { fld = (f_fld, l_fld, d_fld)
                    , frd = (f_frd, r_frd, d_frd)
                    , bld = (b_bld, l_bld, d_bld)
                    , brd = (b_brd, r_brd, d_brd) } = c { fld = (l_bld, b_bld, d_bld)
                                                        , frd = (l_fld, f_fld, d_fld)
                                                        , bld = (r_brd, b_brd, d_brd)
                                                        , brd = (r_frd, f_frd, d_frd) }

rotateCube F' c = rotateCube F $ rotateCube F $ rotateCube F c

rotateCube U' c = rotateCube U $ rotateCube U $ rotateCube U c

rotateCube L' c = rotateCube L $ rotateCube L $ rotateCube L c

rotateCube R' c = rotateCube R $ rotateCube R $ rotateCube R c

rotateCube B' c = rotateCube B $ rotateCube B $ rotateCube B c

rotateCube D' c = rotateCube D $ rotateCube D $ rotateCube D c

rotateCube F2 c = rotateCube F $ rotateCube F c

rotateCube U2 c = rotateCube U $ rotateCube U c

rotateCube L2 c = rotateCube L $ rotateCube L c

rotateCube R2 c = rotateCube R $ rotateCube R c

rotateCube B2 c = rotateCube B $ rotateCube B c

rotateCube D2 c = rotateCube D $ rotateCube D c






