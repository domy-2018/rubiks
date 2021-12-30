
import Test.Tasty
import Test.Tasty.Hspec
import CubeSpec
import MovesSpec
import BeginnerAlgorithmSpec

-- Run all specification tests
main :: IO ()
main = do
    spec_cube <- testSpec "Cube Test" cubeSpecs
    spec_moves <- testSpec "Moves Test" movesSpecs
    spec_balgo <- testSpec "BeginnerAlgorithm Test" beginnerAlgorithmSpecs
    defaultMain (testGroup "Rubiks Test" [spec_cube, spec_moves, spec_balgo])


