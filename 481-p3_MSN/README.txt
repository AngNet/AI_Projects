## Authors
CPSC 481-04, Team MSN
Steven Espin - sespin@csu.fullerton.edu
Marc Jimenez - JimenezMW@csu.fullerton.edu
Natalie Ang - midsummer_season@csua.fullerton.edu

# ACO Path Planning Project

This project is all about Ant Colony Optimization (ACO). All ants start at one part of a 2D 40x60 grid then tries to find a path to the end corner by utilizing ACO swarm. 

Movement is done by using a random number, heuristic value and current scent value. The equation is V(c) = R + H(c) + S(c) where R is random, H(c) = X + Y (cell's location coordinates) and S(c) is the cell's current scent value.

The program runs on iteration. Each iteration has an ant moving onto a neighboring grid cell and for every movement, the ant's cell history is recorded. A scent is placed on the cell the ant was previously on but the scent reduces each iteration. A new ant is also created and placed on the starting cell.

After 40 ants manage to reach the end, all the paths are analyzed to determine which is the best path to take. The program prints the best path length, the best path, and then the completed paths (5 of them). Note that best_path_list in output is in (x y x y x y....) form

## External Requirements

To run the program, first download the lisp program called aco.lisp. A Lisp implementation such as CLISP or Common Lisp is required for the code to run. Common Lisp is available on most operating systems. This project was run on linux. To download Common Lisp on linux through the terminal, type in the following: 

sudo apt-get install common lisp

Using either the terminal or the command line, type in “clisp” to start CLISP and a screen with an ascii clisp logo will show up. This is where you can load and build the lisp program. If you want to exit out of the CLISP environment, type the following:
(bye)

### Instructions for Building and Usage

There are two main ways to build the program and then run it.
1. Go to the aco.lisp, right click the file and select the option "do compile clisp". This will make a compiled clisp file. Right click this new file and  select the option "do load into clisp". This will open up the terminal and start the gp program. 

2. Another option is to open up the terminal and go into the directory with the lisp program before going into the CLISP environment. (Note: If you don’t do this first, you will receive errors when building the program.) Enter the CLISP environment then type in:

(load "aco.lisp”) or (load “aco”)

This will load, build and start the program. (Note: When doing any lisp commands, you must include the parentheses or else the command won't work.)

## Extra Features

The number of completed paths is set to 5 for sake of compile time.To change this, go to line 622 change amount of completed paths found.

To set to another example change (example_2) in framework function to either (example_1) or (example_3).

## Bugs

There is trouble copying path list into best_path_variable since it will occasionally randomly continue to copy values past the point of reaching (39,59). However, path length and best path are still preserved.

Some other stuff to consider is that (evaporate_scent) is commented out in framework for sake of compile time. Evaporation does work along with dissipation on to neighboring cells.

The program also takes some time to run. Depending on which way the user decides to run the program, it can take from 1 - 2 minutes to finish. 
