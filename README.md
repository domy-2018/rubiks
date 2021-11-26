# rubiks
An interactive Haskell imlementation of the Rubiks cube 

Author : Sean 

## Requirements
Allows users to enter in a current cube configuration
    - Maybe also a "quick check" to ensure correct colour orientation, like white opposite yellow, and correct number of edges and corners in the right colours
If you provide a current cube configuration and choose interactive, it will allow you to interactively solve it
    - This will allow you to enter in a move, and then it will print the outcome of the move
Randomize function will start off with a randomized cube, if you don't want to provide initial cube input
    - Randomize by starting from a solved state, then calling a randomize function to provide string of random moves. Then apply those moves.
Option to log the solution to a file


## Aspirational Goals
Validate cube configuration
    - Will it be possible for me to validate the cube configuration? To ensure what is entered is solvable
    - This website provides some info: 
      https://www.quora.com/Is-it-possible-to-determine-whether-a-Rubiks-Cube-is-solvable-just-by-looking-at-its-scrambled-state
If you provide a current cube configuration and the input to solve, then it will solve it for you
    - it will first check to see if it is solvable
    - then solve it and provide the sequence of steps and each of the outcome of the steps

## Design
Using Reader Writer State monads (RWS monad)
Using get options package to get the options from the user
A module to represent the cube data type. 
    - Represent via square, face, edge, corner?
    - Also handles pretty printing it. Maybe using colours? System.Console.ANSI
A module to represent the moves
A main module to take in user parameters, and handle the IO part. 

### Detailed Design

A cube is represented by:
    - 8 corners
    - 12 edges
    - 6 centres

The following is the list of all the corner positions:
    - Front Left Up position (FLU)
    - Front Right Up position (FRU)
    - Front Left Down position (FLD)
    - Front Right Down position (FRD)
    - Back Left Up position (BLU)
    - Back Right Up position (BRU)
    - Back Left Down position (BLD)
    - Back Right Down position (BRD)

The following is the list of all the corner colours:
    - Red White Blue
    - Red White Green
    - Red Blue Yellow
    - Red Green Yellow
    - Orange White Blue
    - Orange White Green
    - Orange Green Yellow
    - Orange Blue Yellow

The following is the list of all the edge positions:
    - Front Up (FU)
    - Left Up (LU)
    - Right Up (RU)
    - Back Up (BU)
    - Front Left (FL)
    - Front Right (FR)
    - Back Left (BL)
    - Back Right (BR)
    - Front Down (FD)
    - Left Down (LD)
    - Right Down (RD)
    - Back Down (BD)

The following is the list of all the edge colours:
    - Red White
    - Red Blue
    - Red Green
    - Red Yellow
    - White Blue
    - White Green
    - Blue Yellow
    - Green Yellow
    - White Orange
    - Blue Orange
    - Green Orange
    - Yellow Orange

The following is the list of all positions of the centre squares:
    - Front (F)
    - Up (U)
    - Left (L)
    - Right (R)
    - Back (B)
    - Down (D)




## Testing
randomize the cube and then run solve.






