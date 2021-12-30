# 2x2 rubiks
An interactive Haskell implementation of the 2x2 Rubiks cube.

This project was developed as part of the Emurgo Blockchain developer course.

Credit to solver algorithm, code is based on the following:
 - Brute force algorithm: https://github.com/KitN/rubiks-two-two
 - Beginner algorithm: https://jperm.net/2x2


## Overview
Allows player to interactively play with a 2x2 rubiks cube displayed in a 2D manner. Player can also choose to enter a batch mode by passing in the -b flag as parameter. By default, game starts interactively.

When you start the game, you can then choose to start with a randomized cube, interactively enter in a starting cube position, start with a solved cube, or start with the cube passed in from the parameters.

Example screenshot below shows the game started interactively and with a randomized cube:

![Interactive start with randomized cube](./img/interactive_start.png)

The log of all the moves and its intermediate states are then logged to a log file, the default filename is rubiks.txt

### Cube representation
In the interactive mode, the cube is displayed in a 2D manner, the different faces of the cube are represented in the following layout:

<pre>
    | U |                U - Up face
    |   |                L - Left face
| L | F | R | B |        F - Front face
|   |   |   |   |        R - Right face
    | D |                B - Back face
    |   |                D - Down face
</pre>


### Valid moves
The following are the valid moves:

```
data Move = F  | U  | R  | F' | U' | R' |
            B  | L  | D  | B' | L' | D' |
            F2 | U2 | R2 | B2 | L2 | D2
```

F rotates the front face 90° clockwise.<br>
The ' is to indicate anti-clockwise rotation.<br>
The 2 is to rotate by 180°.


### Help
<pre>
Usage: rubiks [-b|--batch] [-c|--cube ARG] [-m|--moves ARG] [-l|--log LOGFILE]
  2x2 Rubiks cube game

Available options:
  -b,--batch               Enable batch mode
  -c,--cube ARG            Enter initial cube state for batch mode
  -m,--moves ARG           Enter move list for batch mode
  -l,--log LOGFILE         Write rubiks moves history to LOGFILE
  -h,--help                Show this help text
</pre>

### Examples

Start game interactively:
```
cabal -v0 run rubiks
```
Start game in batch mode passing in a cube starting position and with a list of moves to execute:
```
cabal -v0 run rubiks -- -b -c  "Cube {flu = (O,G,Y), fru = (O,B,Y), fld = (B,O,W), frd = (B,R,W), blu = (R,G,Y), bru = (R,B,Y), bld = (G,O,W), brd = (G,R,W)}" -m "[F, L, D]"
```
Display help message:
```
cabal -v0 run rubiks -- -h
```


## Design
An RWST monad is used where Reader is: [Move], Writer is [(Maybe [Move], Cube)], State is Cube and IO is the base monad.  

The optparse-applicative package is used to get options and parameters from the command line.

Cube.hs - Functions and definitions relating to a cube <br>
Moves.hs - Functions and definitions relating to rotating a cube <br>
Solve.hs - Defines the RWS monad used for solving <br>
BeginnerAlgorithm.hs - Code to solve the cube using the beginner method <br>
BruteForceAlgorithm.hs - Code to solve the cube using the brute force method

### Detailed Design

A 2x2 cube is represented by:
 - 8 corners

The following is the list of all the corner positions:
 - Front Left Up position (flu)
 - Front Right Up position (fru)
 - Front Left Down position (fld)
 - Front Right Down position (frd)
 - Back Left Up position (blu)
 - Back Right Up position (bru)
 - Back Left Down position (bld)
 - Back Right Down position (brd)

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
 - Beginner algorithm
 - Brute force method. (Note this is not used anywhere in the code as it crashes prior to finding the solution when cube is sufficiently randomized)

## Testing
To run the unit tests:
```
cabal -v0 run rubiks-test
```


## Future Development Goals

Some notes on future development to improve the code:
 - Ability to validate cube configuration entered.


