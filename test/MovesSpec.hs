module MovesSpec where

import Test.Hspec
import Moves


movesSpecs :: Spec
movesSpecs = describe "Moves" $ do
                 spec_parseMove
                 spec_parseMoves


spec_parseMove :: Spec
spec_parseMove = describe "parseMove" $ do
                     it "parses F move"  $ parseMove "F" == Just F
                     it "parses U move"  $ parseMove "U" == Just U
                     it "parses L move"  $ parseMove "L" == Just L
                     it "parses R move"  $ parseMove "R" == Just R
                     it "parses B move"  $ parseMove "B" == Just B
                     it "parses D move"  $ parseMove "D" == Just D
                     it "parses F' move" $ parseMove "F'" == Just F'
                     it "parses U' move" $ parseMove "U'" == Just U'
                     it "parses L' move" $ parseMove "L'" == Just L'
                     it "parses R' move" $ parseMove "R'" == Just R'
                     it "parses B' move" $ parseMove "B'" == Just B'
                     it "parses D' move" $ parseMove "D'" == Just D'
                     it "parses F2 move" $ parseMove "F2" == Just F2
                     it "parses U2 move" $ parseMove "U2" == Just U2
                     it "parses L2 move" $ parseMove "L2" == Just L2
                     it "parses R2 move" $ parseMove "R2" == Just R2
                     it "parses B2 move" $ parseMove "B2" == Just B2
                     it "parses D2 move" $ parseMove "D2" == Just D2

spec_parseMoves :: Spec
spec_parseMoves = describe "parseMoves" $ do
                      it "parses moves" $ parseMoves "F F U L R B D D' U' B2" == Just [F, F, U, L, R, B, D, D', U', B2]

