module BeginnerAlgorithmSpec where

import           Test.Hspec
import           Cube
import qualified Moves as M
import           BeginnerAlgorithm
import           Control.Monad.RWS.Strict (execRWS)

-- Run all BeginnerAlgorithm specification tests
beginnerAlgorithmSpecs :: Spec
beginnerAlgorithmSpecs = describe "BeginnerAlgorithm" $ do
                             spec_checkTwoColours
                             spec_isDFaceCorrect
                             spec_beginnerSolve

-- Test the beginnerSolve function
spec_beginnerSolve :: Spec
spec_beginnerSolve = describe "beginnerSolve" $ do
                         it "solves Cube with beginner algorithm" (isItSolved c)
  where
    c :: Cube
    c = let (cres, _) = execRWS beginnerSolve randCube randCube
        in cres

    randCube :: Cube
    randCube = rotateMovesCube [M.F, M.U2, M.B2, M.L', M.D2, M.F, M.R] initCube

-- Test the checkTwoColours function
spec_checkTwoColours :: Spec
spec_checkTwoColours = describe "checkTwoColours" $ do
                           it "two colours are in triple" $ checkTwoColours Y W (W, B, Y)

-- Tests the isDFaceCorrect function
spec_isDFaceCorrect :: Spec
spec_isDFaceCorrect = describe "isDFaceCorrect" $ do
                          it "checks D face is correct" $ isDFaceCorrect O initCube  

