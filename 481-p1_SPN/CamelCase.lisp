;Author       : Steven Espin, Natalie Ang, Payne lacsamana
;Contact Info :	sespin@csu.fullerton.edu
;team         :	spn
;description  :	Program to find the largest coverage path of the camel fairy chess piece in a 7x7 board.
;				Landing on a square that has been already visited in a given path is prohibited.
;				Starting on position (3,2) gets a path of length 24, while starting on (0,0) gets 17.
;				That is, starting position matters.

					;;;board position (x,y) in 7x7 board 
(defvar startX 3)	;;;change starting X here. Must be from 0-6
(defvar startY 2)	;;;change starting Y here. Must be from 0-6

(defvar *spacesCovered* 0)
(defvar solutionPath (list nil))


;;CamelCover uses backtracking to find largest coverage path
(defun CamelCover (boardState x y currentCovered currentPath)

	;;;if spot(x,y) of board is valid and not visited, mark as visited
	(setf (aref boardState x y) 1) 
	(setf currentCovered (+ currentCovered 1))
	(setf currentPath (append currentPath (list(list x y) '>)))
	
	
	;;;recursive calls here, only if next step is valid
	(if (= (validBoard boardState (+ x 3) (+ y 1)) 1)
		(progn 
			(CamelCover boardState (+ x 3) (+ y 1) currentCovered currentPath)
			(setf (aref boardState (+ x 3) (+ y 1)) 0)))	
	
	(if (= (validBoard boardState (+ x 3) (- y 1)) 1)
		(progn 
			(CamelCover boardState (+ x 3) (- y 1) currentCovered currentPath)
			(setf (aref boardState (+ x 3) (- y 1)) 0)))
		
	(if (= (validBoard boardState (- x 3) (+ y 1)) 1)
		(progn 
			(CamelCover boardState (- x 3) (+ y 1) currentCovered currentPath)
			(setf (aref boardState (- x 3) (+ y 1)) 0))) 
		
	(if (= (validBoard boardState (- x 3) (- y 1)) 1)
		(progn 
			(CamelCover boardState (- x 3) (- y 1) currentCovered currentPath)
			(setf (aref boardState (- x 3) (- y 1)) 0)))
		
	
	(if (= (validBoard boardState (+ x 1) (+ y 3)) 1)
		(progn 
			(CamelCover boardState (+ x 1) (+ y 3) currentCovered currentPath)
			(setf (aref boardState (+ x 1) (+ y 3)) 0)))
	
	(if (= (validBoard boardState (- x 1) (+ y 3)) 1)
		(progn 
			(CamelCover boardState (- x 1) (+ y 3) currentCovered currentPath)
			(setf (aref boardState (- x 1) (+ y 3)) 0)))
	
	(if (= (validBoard boardState (+ x 1) (- y 3)) 1)
		(progn 
			(CamelCover boardState (+ x 1) (- y 3) currentCovered currentPath)
			(setf (aref boardState (+ x 1) (- y 3)) 0)))
	
	(if (= (validBoard boardState (- x 1) (- y 3)) 1)
		(progn 
			(CamelCover boardState (- x 1) (- y 3) currentCovered currentPath)
			(setf (aref boardState (- x 1) (- y 3)) 0)))
			
	;;;make sure that if this path is the best path then set it as the best path 
	(if (> currentCovered *spacesCovered*)
		(progn
			(setf *spacesCovered* currentCovered)
			(setf solutionPath currentPath))))
	

;;;if x and y are valid position in board, return 1
(defun validBoard (boardS posX posY)
	(setf trueFalse 0)
	;;;check the bounds of posX and posY
	(if (and (>= posX 0) (<= posX 6) (>= posY 0) (<= posY 6)) 
		(if (= (aref boardS posX posY) 0) (setf trueFalse 1)))
	trueFalse)	;;;return trueFalse
	
		
;;;define start state of board and call CamelCover		
(defun startState ()
	(format t "~%~%~%~%*******************camel cover problem**********************~%")
	
	(if (and (>= startX 0) (<= startX 6) (>= startY 0) (<= startY 6))
		
		;;;if x and y are in bounds
		(progn 
			(setf initialBoard (make-array '(7 7)
			:initial-contents '((0 0 0 0 0 0 0)
								(0 0 0 0 0 0 0)
								(0 0 0 0 0 0 0)
								(0 0 0 1 0 0 0)
								(0 0 0 0 0 0 0)
								(0 0 0 0 0 0 0)
								(0 0 0 0 0 0 0))))	
	
			(setf *spacesCovered* 0)
			(setf path (list 'start))
	
			(CamelCover initialBoard startX startY *spacesCovered* path) ;;;board spot (0,0) gets 17 moves
																		 ;;;board spot (3,2) gets 24 moves
			;;;will output most board spaces filled here
			(format t "Most amount of spaces covered: ~a~%~%" *spacesCovered*)
			(setf solutionPath (append solutionPath 'end))
			(format t "~a~%~%~%" solutionPath))
		
		;;;else if X or Y is out of bounds
		(format t "~%ERROR - X and Y must be in range 0-6")))
	
;;;calling StartState
(startState)