# AI_Projects

A collection of programs that were assigned as group projects from my artificial intelligence class.

## 481-p1_SPN Folder (Camel Cover)

Contains a lisp program that finds a path sequence the provides the largest coverage of visited square on a 7x7 doughnut 
chess board. (Note that the center square (at the row column location (3, 3)) is removed.

## 481-p2_MSN (Genetic Programming)

The main function of this program is to create several random expressions (ex. x + 3). More expressions are made from this in the next generation.

Example: 

Let's say that x = 3, y = 1, z = 2, goal = 15 and the expressions are: (x * 5), (18 - y), (3 - z * 2), (y + 32), (7 - x).

If you plug in the values for each expression, only (x * 5) yield the result 15 which is the goal value. This, as a result, makes it  fit to make children.

This entire processes repeats for 50 generations. Throughout all these generations, about 1% of the total expressions go through mutations where one of the components change. 

## 481-p3_MSN (Ant Colony Optimization)

All ants start at one part of a 2D 40x60 grid then tries to find a path to the end corner by utilizing ACO swarm. The program runs on iteration. Each iteration has an ant moving onto a neighboring grid cell and for every movement, the ant's cell history is recorded.

After 40 ants manage to reach the end, all the paths are analyzed to determine which is the best path to take. The program prints the best path length, the best path, and then the completed paths (5 of them). Note that best_path_list in output is in (x y x y x y....) form
