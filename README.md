# rubiks
An interactive Haskell implementation of the 2x2 Rubiks cube 

Author : Sean 

Credit to solver algorithm based on code here:
https://github.com/KitN/rubiks-two-two

https://jperm.net/2x2

## TODO LIST

10 - documentation

11 - Use Text instead of String (optional?)

## Completed TODO

1 - with the params passed in, if batch, run batch game. if interactive, prompt if player wants to start initial cube
    with params passed in, or interactively entered in, or random cube, or with default solved cube

1a - fix the params so that it won't throw error on parse issue. Provide more friendly error message

2 - fix the show of the cube, so that it shows the results in a pretty 2D cube layout

(CANCELLED) 3 - put the logic of the RWST, interactive game and batch game into its own module

4 - Fix the Writer to record both moves and the cube. 

5 - Fix the output to the log, so that it prints everything nicely. For both batch and interactive, always print to log.

5 - change the parse to readMaybe for cube and move

6 - create a randomized cube

7 - create the solver algorithm to solve the cube

8 - create test cases to test the code

(CANCELLED) 9 - benchmark to benchmark solving the cube (optional?)

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

A 2x2 cube is represented by:
    - 8 corners

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

Solver algorithm:
    - Brute force method.
    - Generate all possible moves for the faces: Front, Up, Right. These moves will be: F, U, R, F', U', R'
    - As there are 6 possible moves, and 2x2 Rubiks cube is solvable with 14 moves. The total number of possibilities are 6^14 = 78,364,164,096
    - Once it finds the solution, log the moves to writer, and the intermediate states.  Log the time taken to find the move, and the final state. Print it out to the user.


## Testing
randomize the cube and then run solve.






