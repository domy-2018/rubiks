
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
               option auto
               (    long "cube"
                 <> short 'c'
                 <> value initCube
                 <> help "Enter initial cube state for batch mode" )
           <*>
               option auto
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

