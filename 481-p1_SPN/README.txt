## Authors
CPSC 481-04, Team SPN
Steven Espin - sespin@csu.fullerton.edu
Payne Lacsamana - rlacsamana@csu.fullerton.edu
Natalie Ang - midsummer_season@csu.fullerton.edu

# Camel Cover Puzzle Project

This project is a lisp program called CamelCase.lisp that finds a path 
sequence the provides the largest coverage of visited square on a 7x7 doughnut 
chess board. (Note that the center square (at the row column location (3, 3)) 
is removed. Also, the project only contains this README.txt and CamelCase.lisp files.) 

## External Requirements

To run the program, first download the lisp program called CamelCase.lisp. 
A Lisp implementation such as CLISP or Common Lisp is required for the code 
to run. Common Lisp is available on most operating systems. This project was 
run on linux. To download Common Lisp on linux through the terminal or cmd.exe,
type in the following: 

sudo apt-get install common lisp

To start CLISP, type in “clisp” and a CLISP ascii graphic along with some
copyright information should pop up. This is where you will be building and 
compiling the lisp program.  If you want to exit out of the CLISP environment, 
type the following:

(bye)

### Instructions for Building and Usage

There are two main ways to build the program and then run it.

1. Go to the CamelCase.lisp, right clck the file and select the option "Compile 
CLISP". This will make a compiled clisp file. Right click this new file and 
select the option "load into CLISP". This will open up the terminal and start
the CamelCase Cover. 

(Note: if you don't have common lisp installed, you won't get any of these 
options when right clicking the lisp file)

On the screen, you should see the header "camel cover problem". The program will
take around 40 seconds to run and then show the amount of space covered along
with the path that it took from start to end. To run the program again, type this 
in the CLISP environment:

(startState)

2. Another option is to open up the terminal and go into the directory with the
lisp program before going into the CLISP environment. (Note: If you don’t do 
this first, you will receive errors when building the program.) Enter the CLISP
environment and then type in:

(load “CamelCase.lisp”) or (load “CamelCase”)

This will load, build and start the program. (Note that this will take a much 
longer time for the program to run. It is recommended for the user to compile
and run the program with the first option.) 

## Extra Features

The current starting position on the board is (3,2). The user is allowed to change
the starting position. For example, if user wanted to start at (0,0) use 
(setf startX 0) and (setf startY 0) and then (startSpace) to run again. 
Else, the user is allowed to change directly in code.

## Bugs

There are no run time error or debugging problems in the program. The program is 
not able to traverse the entire 7x7 board. At most, it covers 24 spaces which is
half the number of spaces. Depending on the starting position, the spaces covered
could be 16, 17, or 24.

Also, depending on the starting position, the program can run from 40 seconds to 
2 minutes and 20 seconds before it displays the amount covered and the path it 
took. The amount of time can also vary depending on how the user decides to run
the program. 


