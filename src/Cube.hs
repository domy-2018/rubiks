

module Cube where


import           Prelude         hiding (Left, Right)
import qualified Data.Map.Strict as M


data Centre = Front | Up | Left | Right | Back | Down
    deriving (Show, Ord, Eq)

data Colour = White | Yellow | Red | Orange | Blue | Green
    deriving Show

newtype Cube = Cube { centres :: M.Map Centre Colour }
    deriving Show


initCube :: Cube
initCube = Cube {
               centres = M.fromList [(Front, White), (Back, Yellow), (Left, Green), (Right, Blue), (Down, Red)]
           }
