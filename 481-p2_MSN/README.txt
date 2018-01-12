## Authors
CPSC 481-04, Team SPN
Steven Espin - sespin@csu.fullerton.edu
Marc Jimenez - JimenezMW@csu.fullerton.edu
Natalie Ang - midsummer_season@csua.fullerton.edu

# GP Arithmetic Expression Project

This project is all about genetic programming (GP) and works by utilizing several programs, bit strings, and a tree. The main function of this program is to create several random expressions (ex. x + 3). More expressions are made from this in the next generation.

Possible components to these expressions are as following: Alphabet: Variables X, Y, Z; Integers -9, 8, ..., 8, 9; Operators +, -, *

In each generation, not all expressions are allowed to make more children. Only the ones that are considered "fit" can do so. When determining the fitness of an expression, the values of x, y, and z are given to be plugged in. If the result equals the goal value, it is fit to make children for the next generation. 

Example: 

Let's say that x = 3, y = 1, z = 2, goal = 15 and the expressions are: (x * 5), (18 - y), (3 - z * 2), (y + 32), (7 - x).

If you plug in the values for each expression, only (x * 5) yield the result 15 which is the goal value. This, as a result, makes it  fit to make children.

This entire processes repeats for 50 generations. Throughout all these generations, about 1% of the total expressions go through mutations where one of the components change. 

## External Requirements

To run the program, first download the lisp program called CamelCover.lisp. A Lisp implementation such as CLISP or Common Lisp is required for the code to run. Common Lisp is available on most operating systems. This project was run on linux. To download Common Lisp on linux through the terminal, type in the following: 

sudo apt-get install common lisp

Using either the terminal or the command line, type in “clisp” to start CLISP and a screen with an ascii clisp logo will show up. This is where you can load and build the lisp program. If you want to exit out of the CLISP environment, type the following:
(bye)

### Instructions for Building and Usage

There are two main ways to build the program and then run it.
1. Go to the gp.lisp, right clck the file and select the option "do compile  clisp". This will make a compiled clisp file. Right click this new file and  select the option "do load into clisp". This will open up the terminal and start the gp program. 

2. Another option is to open up the terminal and go into the directory with the lisp program before going into the CLISP environment. (Note: If you don’t do this first, you will receive errors when building the program.) Enter the CLISP environment then type in:

(load “gp.lisp”) or (load “gp”)

This will load, build and start the program. (Note: When doing any lisp commands, you must include the parantheses or else the command won't work.)

## Extra Features

1. If the user is in the clisp environment and has load, build and start the program once, they can type in (framework) to run the program again. Note: Be sure to include the paranthesis.

2. There is a helper function named set-xyz-goal that will allow the user to change the x y z and goal-value. If the user wanted x = 1, y = 2, z = 3 and the goal-value to be 4, type in:

(set-xyz-goal 1 2 3 4)

The next time the program runs, the new values for x, y, z, and goal will be set. This will change what will be considered "fit" for the expressions to make children.(Note: The default values are x = 0, y = -2, z = 1, and goal = -16. If you exit clisp then come back and run the gp program, the default values will be used)

Another way to change the values is to manually go into the gp.lisp program and change the values. The variable initialization can be found in lines 494-487.

## Bugs

There are no known bugs to be in this program. There are some occasional warnings depending on how you run the lisp program, but it doesn't effect the overall functionality of it.