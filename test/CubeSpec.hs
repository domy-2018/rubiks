module CubeSpec where


import           Test.Hspec
import           Cube
import qualified Moves as M

cubeSpecs :: Spec
cubeSpecs = describe "Cube" $ do
                spec_oppositeColour
                spec_rotateCube
                spec_isItSolved
                spec_parseColour
                spec_parseCorner

-- tests the oppositeColour function
spec_oppositeColour :: Spec
spec_oppositeColour = describe "oppositeColour" $ do
                        it "opposite colour of W is Y" $ oppositeColour W == Y
                        it "opposite colour of Y is W" $ oppositeColour Y == W
                        it "opposite colour of B is G" $ oppositeColour B == G
                        it "opposite colour of G is B" $ oppositeColour G == B
                        it "opposite colour of R is O" $ oppositeColour R == O
                        it "opposite colour of O is R" $ oppositeColour O == R

-- tests the rotateCube function
spec_rotateCube :: Spec
spec_rotateCube = describe "rotateCube" $ do
                    it "rotates F correctly" $ rotateCube M.F initCube == rotateCube M.F' (rotateCube M.F2 initCube)
                    it "rotates U correctly" $ rotateCube M.U initCube == rotateCube M.U' (rotateCube M.U2 initCube)
                    it "rotates L correctly" $ rotateCube M.L initCube == rotateCube M.L' (rotateCube M.L2 initCube)
                    it "rotates R correctly" $ rotateCube M.R initCube == rotateCube M.R' (rotateCube M.R2 initCube)
                    it "rotates B correctly" $ rotateCube M.B initCube == rotateCube M.B' (rotateCube M.B2 initCube)
                    it "rotates D correctly" $ rotateCube M.D initCube == rotateCube M.D' (rotateCube M.D2 initCube)
                    

-- tests the isItSolved function
spec_isItSolved :: Spec
spec_isItSolved = describe "isItSolved" $ do
                      it "initCube is solved" $ isItSolved initCube

-- tests the parseColour function
spec_parseColour :: Spec
spec_parseColour = describe "parseColour" $ do
                       it "parses colour W" $ parseColour "W" == Just W
                       it "parses colour Y" $ parseColour "Y" == Just Y
                       it "parses colour R" $ parseColour "R" == Just R
                       it "parses colour O" $ parseColour "O" == Just O
                       it "parses colour B" $ parseColour "B" == Just B
                       it "parses colour G" $ parseColour "G" == Just G

-- tests the parseCorner function
spec_parseCorner :: Spec
spec_parseCorner = describe "parseCorner" $ do
                       it "parses corners correctly" $ parseCorner "Y G R" == Just (Y, G, R)



