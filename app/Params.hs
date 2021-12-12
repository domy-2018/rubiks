
module Params where

import Options.Applicative
import Cube
import Moves

data Mode = Interactive | Batch
    deriving Show

data Params = Params {
                mode       :: Mode
              , cubeParams :: Cube
              , moveParams :: [Move]
              , logFile    :: FilePath
              }
    deriving Show

mkParams :: Parser Params
mkParams = Params
           <$>
               flag Interactive Batch
               (    long "batch"
                 <> short 'b'
                 <> help "Enable batch mode" )
           <*>
               option (myauto " \n\nExample of correct cube value: \"Cube {flu = (W,B,R), fru = (W,G,R), fld = (W,B,O), frd = (W,G,O), blu = (Y,B,R), bru = (Y,G,R), bld = (Y,B,O), brd = (Y,G,O)}\"")
               (    long "cube"
                 <> short 'c'
                 <> value initCube
                 <> help "Enter initial cube state for batch mode" )
           <*>
               option (myauto " \n\nExample of correct move list value: \"[F, U, L, R, B, D, F', F2]\" etc.")
               (    long "moves"
                 <> short 'm'
                 <> value []
                 <> help "Enter move list for batch mode" )
           <*>
               strOption
               (    long "log"
                 <> short 'l'
                 <> metavar "LOGFILE"
                 <> value "rubiks.txt"
                 <> help "Write rubiks moves history to LOGFILE" )

cmdLineParser :: IO Params
cmdLineParser = execParser opts
  where
    opts = info (mkParams <**> helper)
                (fullDesc <> progDesc "2x2 Rubiks cube game")



myauto :: Read a => String -> ReadM a
myauto err = eitherReader $ \arg -> case reads arg of
  [(r, "")] -> return r
  _         -> Left $ "Invalid value: \"" ++ arg ++ "\"." ++ err
