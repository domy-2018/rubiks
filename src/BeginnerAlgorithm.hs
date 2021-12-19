{-# LANGUAGE RecordWildCards #-}

module BeginnerAlgorithm where



import Cube  as C
import Moves as M

-- eventually I think I need to use RWS, and then return W as the full list of moves.
-- but as a start, let's just try Cube to Cube to get my algorithm working 


-- uses the Beginner method to solve
beginnerSolve :: Cube -> [Move]
beginnerSolve = undefined


-- step three - solve the last layer
-- firstly run F2 and B2 to re-orient the U to D and D to U. This will bring the unsolved layer to the bottom
-- firstly check the FRD if the correct colour is at the D position. If it is, then rotate the entire cube with U' D until the D face of FRD is not the right colour
-- if the D position is not the correct colour run the swapFRUFRD moves until the D position colour is correct
-- then check the entire D face is correct or not, if not run D to bring the next FRD to be corrected.
-- repeat until all 4 positions are correct, then run D until the cube is solved.

-- check what the D colour of FRD is, and find the opposite colour.
-- The opposite of this colour will be what the colour is to solve for the last Face.
solveLastLayer :: Cube -> Cube
solveLastLayer c@Cube { frd = (_, _, d_frd) } = runSolveLastLayer (oppositeColour d_frd) (rotateMovesCube [F2, B2] c)

-- the Colour passed in here, is the expected colour of the face to be solved
runSolveLastLayer :: Colour -> Cube -> Cube
runSolveLastLayer cd c@Cube { frd = (_, _, d_frd) }
    | isItSolved c         = c
    | isDFaceCorrect cd c  = runSolveLastLayer cd (rotateCube D c)
    | cd == d_frd          = runSolveLastLayer cd (rotateCube D c)
    | otherwise            = runSolveLastLayer cd (recursiveStep3DFace cd c)


-- keep running recursiveStep3FRD until the entire D face is correct
-- note: this assumes the first FRD is not in the correct position
recursiveStep3DFace :: Colour -> Cube -> Cube
recursiveStep3DFace cd c
    | isDFaceCorrect cd c = c
    | otherwise           = recursiveStep3DFace cd $ rotateCube D (recursiveStep3FRD cd c)



-- keep running swapFRUFRD until the D face of FRD is correct
-- the D face of FRD is correct when it matches the colour passed in to this function
recursiveStep3FRD :: Colour -> Cube -> Cube
recursiveStep3FRD cd c@Cube { frd = (_, _, d_frd) }
    | cd == d_frd       = c
    | otherwise         = recursiveStep3FRD cd (swapFRUFRD c)


-- this checks the entire D face has the correct colour opposite to U
isDFaceCorrect :: Colour -> Cube -> Bool
isDFaceCorrect cd Cube {fld = (_, _, d_fld),
                        frd = (_, _, d_frd),
                        bld = (_, _, d_bld),
                        brd = (_, _, d_brd)} = d_fld == cd &&
                                               d_frd == cd &&
                                               d_bld == cd &&
                                               d_brd == cd

-- this checks the D position of the FRD has the correct colour opposite to the U
-- INCORRECT ---> I need to pass in a colour to check at the start because the upper corners will be screwed.
{-
isDofFRDcorrect :: Cube -> Bool
isDofFRDcorrect Cube {fru = (_, _, u_fru),
                      frd = (_, _, d_frd)}
    | (u_fru == Y   && d_frd == W) || (u_fru == W && d_frd == Y)   = True
    | (u_fru == C.B && d_frd == G) || (u_fru == G && d_frd == C.B) = True
    | (u_fru == C.R && d_frd == O) || (u_fru == O && d_frd == C.R) = True
    | otherwise                                                    = False
-}




-- step two - correctly position the top pieces
-- firstly check that the FRU position is correct. if it is, then check the other 3 corners.
--     - if all positions are correct, then you're done
--     - if none of the other positions are correct, run the reposition move and then check corners again
--     - if FLU position is correct, and BLU and BRU position is incorrect, then run U to position BLU to the correct position and then run U and D' to reorient the entire cube so that BLU is now in the FRU position, then run the reposition move and then check corners again.
--     - if the BLU position is correct, and FLU, BRU position incorrect, then run reposition move and check again
--     - if the BRU position is correct, and FLU, BLU position incorrect, then run U' to reposition BLU to FLU which is its correct position, then run U' D to re-orient the entire cube so that it is in the FRU position
-- if FRU position is incorrect, run U then check again
repositionTopPieces :: Cube -> Cube
repositionTopPieces c@Cube {fld = (f_fld, l_fld, _),
                            frd = (f_frd, r_frd, _),
                            bld = (b_bld, l_bld, _),
                            brd = (b_brd, r_brd, _)}
    | checkTwoColours f_frd r_frd (fru c) &&
      checkTwoColours f_fld l_fld (flu c) &&
      checkTwoColours l_bld b_bld (blu c) &&
      checkTwoColours r_brd b_brd (bru c)          = c
    | checkTwoColours f_frd r_frd (fru c)       &&
      not (checkTwoColours f_fld l_fld (flu c)) &&
      not (checkTwoColours l_bld b_bld (blu c)) &&
      not (checkTwoColours r_brd b_brd (bru c))    = repositionTopPieces (repositionFLUBRUBLU c)
    | checkTwoColours f_frd r_frd (fru c)       &&
      checkTwoColours f_fld l_fld (flu c)       &&
      not (checkTwoColours l_bld b_bld (blu c)) &&
      not (checkTwoColours r_brd b_brd (bru c))    = repositionTopPieces $ repositionFLUBRUBLU (rotateMovesCube [U, U, D'] c)
    | checkTwoColours f_frd r_frd (fru c)       &&
      not (checkTwoColours f_fld l_fld (flu c)) &&
      checkTwoColours l_bld b_bld (blu c)       &&
      not (checkTwoColours r_brd b_brd (bru c))    = repositionTopPieces (repositionFLUBRUBLU c)
    | checkTwoColours f_frd r_frd (fru c)       &&
      not (checkTwoColours f_fld l_fld (flu c)) &&
      not (checkTwoColours l_bld b_bld (blu c)) &&
      checkTwoColours r_brd b_brd (bru c)          = repositionTopPieces $ repositionFLUBRUBLU (rotateMovesCube [U', U', D] c)
    | otherwise                                    = repositionTopPieces (rotateCube U c)




-- this set of moves will move FLU BRU BLU in anticlockwise manner to reposition
repositionFLUBRUBLU :: Cube -> Cube
repositionFLUBRUBLU = rotateMovesCube [U, M.R, U', L', U, R', U', L]

-- fix the colour on the BRD.
-- if the colour I want is on either FRU or FRD, then run swapFRUFRD until it is in the FRD in the correct orientation
-- if the colour I want is on U face, then rotate it to FRU and run swapFRUFRD until I get it right
-- if the colour I want is FLD, then do F', to get it to FRD, then run swapFRUFRD until I get it right
-- if the colour I want is BLD, then do L' F', to get it to FRD, then run swapFRUFRD until it is right
-- once both BRU and FRD is right, then do D to move it away, and work on the next colour
-- now the only position it can be is if it is U, or if it is FLD. If FLD, run F' then swapFRUFRD.
-- if it is in FRU or FRD, then run swapFRUFRD, if anywhere else, then rotate U to get it to FRU, then swapFRUFRD
-- then run D again to move it away.
-- last piece is either on U face, or FRU, FRD, do the same as above then run swapFRUFRD until it is right.
-- At this point the bottom layer should be solved.
solveBottomLayer :: Cube -> Cube
solveBottomLayer c
    | isBottomLayerSolved c = c
    | otherwise             = solveBottomLayer $ rotateCube D (solveFRD c)


-- checks to see if the bottom layer is solved 
-- all down colours are the same
-- all the right side colours, front side, left side and back side are the same
isBottomLayerSolved :: Cube -> Bool
isBottomLayerSolved Cube { frd = (f_frd, r_frd, d_frd),
                           fld = (f_fld, l_fld, d_fld),
                           brd = (b_brd, r_brd, d_brd),
                           bld = (b_bld, l_bld, d_bld) } = d_frd == d_fld && d_frd == d_brd && d_frd == d_bld &&
                                                           r_frd == r_brd &&
                                                           f_frd == f_fld &&
                                                           l_fld == l_bld &&
                                                           b_brd == b_bld
      

-- given a cube check the colours of brd.
-- run reposition and recursiveSolve to solve the frd position
solveFRD :: Cube -> Cube
solveFRD c@Cube { brd = (_, r_brd, d_brd) } = recursiveSolveFRD r_brd d_brd (repositionFRUFRD r_brd d_brd c)


-- given two colours, look for the corner which contains that colour, and reposition it to either FRU or FRD
repositionFRUFRD :: Colour -> Colour -> Cube -> Cube
repositionFRUFRD c1 c2 c@Cube {..}
    | checkTwoColours c1 c2 flu = rotateCube U' c
    | checkTwoColours c1 c2 bru = rotateCube U  c
    | checkTwoColours c1 c2 blu = rotateCube U2 c
    | checkTwoColours c1 c2 fld = rotateCube F' c
    | checkTwoColours c1 c2 bld = rotateMovesCube [L', F'] c
    | otherwise                 = c -- otherwise it is in fru and frd

-- checks two colours are in a triple
checkTwoColours :: Colour -> Colour -> (Colour, Colour, Colour) -> Bool
checkTwoColours c1 c2 (c3, c4, c5) = c1 `elem` [c3, c4, c5] && c2 `elem` [c3, c4, c5]


-- first colour is R, second colour is D, and given a cube
-- keep running swapFRUFRD until FRD is solved
-- this function assumes that the corner containing the two colours are either in FRU or FRD position
recursiveSolveFRD :: Colour -> Colour -> Cube -> Cube
recursiveSolveFRD cr cd c@Cube { frd = (_, r_frd, d_frd) }
    | r_frd == cr && d_frd == cd = c
    | otherwise                  = recursiveSolveFRD cr cd (swapFRUFRD c)


-- this swaps FRU and FRD, and swaps BLU and BRU. 
-- FLU, FRD, FLD, BLD and BRD does not change.
swapFRUFRD :: Cube -> Cube
swapFRUFRD = rotateMovesCube [M.R, U, R', U']


