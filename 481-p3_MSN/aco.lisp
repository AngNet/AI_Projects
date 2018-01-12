;Author       : Marc Jimenez, Steven Espin, Natalie Ang 
;Contact Info :	sespin@csu.fullerton.edu
;team         :	msn
;description  :	ant colony optimization. place ants to find paths starting at (0,0) and ending at (39,59)				

;framework ()
 "framework for Ant Colony Optimization algorithm."
;set_grid (G)
 "sets our grid from (0 0) to (39 59)"
;make_grid_cell (X Y)
 "make a cell for corresponding X and Y coordinate" 
;set_bar (X Y dir len)
 "set bar/obstacles on map at (x,y)"
;new_ant (ant_list)
 "create a new ant and add it to the ants list"
;move_ant (ant)
 "move ant in up, down, left, or right direction depending on (hueristic + scent + random) cell values"
;calculate_cell_score (x y)
 "return (hueristic + scent + random)"
;add_to_path_history (ant ant_x ant_y)
 "add x and y to the ants path history element"
;add_to_tabu_list (ant ant_x ant_y)
 "add x and y to the ants tabu list"
;is_tabu_cell (ant potential_x potential_y)
 "return true if the potential (x,y) location is in tabu list of this ant"
;check_if_all_tabu (ant new_x new_y)
 "check to see if all potential (x,y) locations are in tabu list, picking least recently used cell"
;place_scent (X Y) 
 "place scent value at (x,y)"
;evaporate_scent ()
 "evaporate scent across all cells in the grid, note: drastically increases run-time"
;remove_ant () 
 "remove an ant who has recently reached (39,59) in grid"
;best_path ()
 "output best path out of completed_paths"
;example_1 & example_2 & example_3
 "change grid depending on example function called, note: do not call more than one at a time"
 
(setf *random-state* (make-random-state t)) ;;;for random states
;; ------------------------------------------------- make_grid_cell ---- 
(defun make_grid_cell (X Y)
	(setf cell '())
	;;cell in form of (x y Hueristic scent isBar)
	
	;;set X and Y coordinate
	(setf cell (append cell (list X)))
	(setf cell (append cell (list Y)))
	
	;;set Hueristic (X + Y)
	(setf cell (append cell (list (+ X Y))))
	
	;;set scent value (initially zero)
	(setf cell (append cell (list 0)))
	
	;;set isBar boolean (initially zero)
	(setf cell (append cell (list 0)))
	
	cell)

;; Tests
;; (make_grid_cell 1 1)
;; (1 1 2 0 0)
;; (make_grid_cell 20 40)
;; (20 40 60 0 0)

;; ------------------------------------------------- set_grid ----
(defun set_grid (G)
	;set grid from (0 0) to (39 59) 
	(loop for x from 0 to 39 do
		(loop for y from 0 to 59 do
			(setf G (append G (list (make_grid_cell x y))))))		
	G)

 
 
;; ------------------------------------------------- set_bar ----
(defun set_bar (X Y dir len)
	
	;;The list is still a 1-D list, so every cell is a 1-D nth element
	;;Due to how the grid was made, all the Y values are listed first
	;;So in order to go by the Xth value, we must multiply it by 60, as
	;;the Y values stop at 59
	
	(setq x_con (* 60 X))
	(if (= dir 1)
		;;based on X direction
		(progn
			;;down direction
			(loop for i from 0 to ( - len 1) do
				(setq fin_len (+ (* 60 i) x_con))
				(setf (nth 4 (nth (+ fin_len Y)grid)) 1)
				;(print (nth(+ fin_len Y)grid))
				))
		(progn
			;;right direction
			(loop for i from 0 to ( - len 1) do
				(setf (nth 4 (nth (+ (* X 60) (+ Y i))grid )) 1)
				;(print (nth(+ (* X 60) (+ Y i))grid))
				))))
				
;; ------------------------------------------------- new_ant ----				
				
(defun new_ant (ant_list)
		(setf ant_list (append ant_list (list (list 0 0 '(0 0) '(0 0) 0))))
		;;ant in form of (X Y (last 6 places X Y X Y...) (Path History X Y X Y...) PathLength)
		ant_list)
		
		
;; ------------------------------------------------- move_ant ----
	
(defun move_ant (ant)	
	(setq ant_x (nth 0 ant))
	(setq ant_y (nth 1 ant))
	(setq new_x ant_x)
	(setq new_y ant_y)
	(setq curr_best 0) ;;will keep track of highest "score"
	(setq temp_score 0)
	(setq x_con (* 60 ant_x))
	
	;;TODO: add fuction to check tabu list before moving to spot
	
	;;check up
	(if (> ant_x 0) ;;bounds checking
		(if (= (is_tabu_cell ant (- ant_x 1) ant_y) 0)
		;;check for bar 
		(if (= (nth 4 (nth (- (+ x_con ant_y) 60) grid)) 0)
			(progn 
			(setf temp_score (calculate_cell_score (- ant_x 1) ant_y))	;;get "score" for cell
			(if (> temp_score curr_best) 
				(progn 
				(setf curr_best temp_score)
				(setf new_x (- ant_x 1))
				(setf new_y ant_y)))))))
	;;check right
	(if (< ant_y 59) ;;bounds checking
		(if (= (is_tabu_cell ant ant_x (+ ant_y 1)) 0)
		;;check for bar 
		(if (= (nth 4 (nth (+ (+ x_con ant_y) 1) grid)) 0)
			(progn 
			(setf temp_score (calculate_cell_score ant_x (+ ant_y 1)))  ;;get "score" for cell
			(if (> temp_score curr_best) 
				(progn 
				(setf curr_best temp_score)
				(setf new_x ant_x)
				(setf new_y (+ ant_y 1))))))))
	;;check down
	(if (< ant_x 39) ;;bounds checking
		(if (= (is_tabu_cell ant (+ ant_x 1) ant_y) 0)
		;;check for bar 
		(if (= (nth 4 (nth (+ (+ x_con ant_y) 60) grid)) 0)
			(progn 
			(setf temp_score (calculate_cell_score (+ ant_x 1) ant_y))	;;get "score" for cell
			(if (> temp_score curr_best) 
				(progn 
				(setf curr_best temp_score)
				(setf new_x (+ ant_x 1))
				(setf new_y ant_y)))))))
	;;check left
	(if (> ant_y 0) ;;bounds checking
		(if (= (is_tabu_cell ant ant_x (- ant_y 1)) 0)
		;;check for bar 
		(if (= (nth 4 (nth (- (+ x_con ant_y) 1) grid)) 0)
			(progn 
			(setf temp_score (calculate_cell_score ant_x (- ant_y 1)))
			(if (> temp_score curr_best) ;;get "score" for cell
				(progn 
				(setf curr_best temp_score)
				(setf new_x ant_x)
				(setf new_y (- ant_y 1))))))))
	
	;;check if all cells ended up being in tabu list
	;;choose least recently used, otherwise simply change to new x and y 
	(if (= (check_if_all_tabu ant new_x new_y) 0)
		(progn 
			;;changing x and y location of ant
			(setf (nth 0 ant) new_x)	
			(setf (nth 1 ant) new_y)))
	
	;;add new (x,y) to ants history
	(add_to_path_history ant (nth 0 ant) (nth 1 ant))
	
	;;DBG: print new_x and new_y
	;(format t "~%next(x,y): ~a ~a~%" (nth 0 ant) (nth 1 ant))
	
	;;add to tabu list
	(add_to_tabu_list ant (nth 0 ant) (nth 1 ant))
	
	;;increment path length
	(setf (nth 4 ant) (+ (nth 4 ant) 1))
	
	;;check if Goal found 
	(if (and (= (nth 0 ant) 39) (= (nth 1 ant) 59))
		(remove_ant))
)
	
;; ------------------------------------------------- calculate_cell_score ----

(defun calculate_cell_score (x y)
	(setq score 0)
	(setq heuristic 0)
	(setq scent 0)
	(setq rand 0)
	
	(setq index (+ (* 60 x) y)) ;;used to access grid cell
	
	;;set hueristic from corresponding x and y in grid
	(setf heuristic (nth 2 (nth index grid)))
	
	;;set scent from corresponding x and y in grid
	(setf scent (nth 3 (nth index grid)))
	
	;;set random value from 0-40
	(setf rand (random 40))
	
	;;add up score
	(setf score (+ rand heuristic scent))
	
	;;DBG: print results
	;(format t "~%hueristic: ~a~%" heuristic)
	;(format t "~%scent: ~a~%" scent)
	;(format t "~%rand: ~a~%" rand)
	;(format t "~%score: ~a~%~%" score)
	
	score ;;return score
)

;; ------------------------------------------------- add_to_path_history ----

(defun add_to_path_history (ant ant_x ant_y)
	(setq path_list (nth 3 ant))
	(setf path_list (append path_list (list ant_x ant_y)))	;;append (x,y)
	(setf (nth 3 ant) path_list)
)

;; ------------------------------------------------- add_to_tabu_list ----

(defun add_to_tabu_list (ant ant_x ant_y) 
	(setq tabu (nth 2 ant))
	
	(if (< (list-length tabu) 12)
		(setf tabu (append tabu (list ant_x ant_y)))	;;then
		
		(progn 	;;else
			(setf tabu (append tabu (list ant_x ant_y))) ;;add pair
			(setf tabu (nthcdr 2 tabu))					 ;;remove pair
		)		
	)
	
	(setf (nth 2 ant) tabu))
	
;; ------------------------------------------------- add_to_tabu_list ----

(defun is_tabu_cell (ant potential_x potential_y) 
	(setq tabu (nth 2 ant))
	(setq is_tabu 0)
	
	(loop for i from 0 to (- (list-length tabu) 1) do 
		(if (evenp i)
			(if (and (= potential_x (nth i tabu)) (= potential_y (nth (+ i 1) tabu)))
				(progn 
				(setf is_tabu 1)))))
	is_tabu	 ;;return boolean
)

;;tests
;;(is_tabu_cell '(0 0 (1 0) (0) 0) 1 0)
;;1
;;(is_tabu_cell '(0 0 (1 0 0 1 2 3 4 5) (0) 0) 0 0)
;;0
;;(is_tabu_cell '(0 0 (1 0 0 1 2 3 4 5 1 1 0 0) (0) 0) 0 0)
;;1

;; ------------------------------------------------- check_if_all_tabu ----

(defun check_if_all_tabu (ant new_x new_y)
	(setq tabu (nth 2 ant))
	(setq found_least_recent 0)
	
	(if (and (= (nth 0 ant) new_x) (= (nth 1 ant) new_y))
		;we didn't change x or y, meaning all possible moves where in tabu list
		(loop for i from 0 to (- (list-length tabu) 1) do 
			(if (evenp i)
				;look for matching pair in tabu list, either up right down or left
				;up
				(progn 
				(if (and (= (+ new_x 1) (nth i tabu)) (= new_y (nth (+ i 1) tabu)) (= found_least_recent 0))
					(progn	
					(setf (nth 0 ant) (+ new_x 1))	;;force a move
					(setf found_least_recent 1)))
				;down 
				(if (and (= (- new_x 1) (nth i tabu)) (= new_y (nth (+ i 1) tabu)) (= found_least_recent 0))
					(progn
					(setf (nth 0 ant) (- new_x 1))	;;force a move
					(setf found_least_recent 1)))
				;right
				(if (and (= new_x (nth i tabu)) (= (+ new_y 1) (nth (+ i 1) tabu)) (= found_least_recent 0))
					(progn 
					(setf (nth 1 ant) (+ new_y 1))	;;force a move
					(setf found_least_recent 1)))
				;left
				(if (and (= new_x (nth i tabu)) (= (- new_y 1) (nth (+ i 1) tabu)) (= found_least_recent 0))
					(progn
					(setf (nth 1 ant) (- new_y 1))	;;force a move
					(setf found_least_recent 1))))
		
			)	
		)
	)
	found_least_recent
)

;; ------------------------------------------------- place_scent ----

;;Simple function to place scent at the location
(defun place_scent (X Y) 
	
	(setf (nth 3 (nth (+ (* 60 X) Y)grid)) 20 )
	
	
	;;Following are debug statements
	;(format t "Scent at  ~a,~a : ~%" X Y )
	;(print (nth(+ (* 60 X) Y)grid))
	;(format t "~%")
)

;; ------------------------------------------------- evaporate_scent ----

(defun evaporate_scent () 

	;These values refer to the scent reduction (which will eventually be scent/evaporation)
	;evaporation, which is by default is 20 for the project, can be modified
	;and spread scent, which by the project standards, will be scent reduction/5, which will
	;spread to surrounding cells
	(setq scent_reduction 0)
	(setq evaporation 20)
	(setq spread_scent 0)
	
	;Having new values to represent cells to the left/right/up/down
	;from the current cell location
	(setq left 0)
	(setq right 0)
	(setq up 0)
	(setq down 0)

	;Check to see if scent reduction starts at 0
	;;(format t "Scent reduction is:  ~a~%" scent_reduction )
	(loop for i from 0 to 2399 do
		(if (< (nth 3 (nth i grid)) 1)
			(progn
				(setf scent_reduction (nth 3 (nth i grid)))
				;;(format t "Scent reduction is:  ~a~%" scent_reduction )
				(setf (nth 3 (nth i grid)) (- (nth 3 (nth i grid)) scent_reduction))
				
				))
				
	)
	
	(loop for i from 0 to 2399 do
		(if (>= (nth 3 (nth i grid)) 1)
			(progn
				;;Does the math needed to reduce the scent in the cell
				;;and to have the value for the scent added to surrounding cells
				(setf scent_reduction (/ (nth 3 (nth i grid) )20))
				;(format t "Scent reduction is:  ~a~%" scent_reduction )
				(setf (nth 3 (nth i grid)) (- (nth 3 (nth i grid)) scent_reduction))
				(setf spread_scent (/ scent_reduction 5))
				;(format t "spread_scent is:  ~a~%" spread_scent )
				
				;;Sets left/right/up/down based on current cell
				(setf left (- i 60))
				(setf right (+ i 60))
				(setf up (- i 1))
				(setf down (+ i 1))
				
				;;Add spread_scent to the the left/right/up/down cells, only if the cell is
				;;located in the grid
				(if (and (> left 0) 
					(< left 2400)) 
						(setf (nth 3 (nth left grid)) (+ (nth 3 ( nth left grid)) spread_scent))
				)
				(if (and (> right 0) 
					(< right 2400)) 
						(setf (nth 3 (nth right grid)) (+ (nth 3 ( nth right grid)) spread_scent))
				)
				(if (and (> up 0) 
					(< up 2400)) 
						(setf (nth 3 (nth up grid)) (+ (nth 3 ( nth up grid)) spread_scent))
				)
				(if (and (> down 0) 
					(< down 2400)) 
						(setf (nth 3 (nth down grid)) (+ (nth 3 ( nth down grid)) spread_scent))
				)
				
				;Debug statement to check specific cells
				;;(print (nth left grid))
				
			)		
		)
	)
)

;; ------------------------------------------------- remove_ant ----

(defun remove_ant () 
	;;If called without an ant in the goal location of (39,59), the program will break
	
	;;Test statements to place an ant at the goal state.
	;;Note: this will edit the current position
	;;of the first ant in the list to be at 39,59
	;;disregarding the path the ant currently was taking
	
	;(setf (nth 0 (nth 0 ants))  39) 
	;(setf (nth 1 (nth 0 ants))  59) 
	;(print (nth 0 ants))
	;(format t "~%")
	
	
	;;The following are values used in the loop to increment and 
	;;test each ant if it is the ant that is meant to be removed
	;;and also to stop looping once an ant is removed.
	;;The assumption is that one ant will be at 39,59 at a time
	(setq no_check 0)
	(setq inc_val 0)
	(loop do
        (if (and (= (nth 0 (nth inc_val ants))  39) 
			(= (nth 1 (nth inc_val ants)) 59)) 
				(progn
					;; first add the path history and path length of the ant to the completed_paths
					(setf completed_paths (append completed_paths (list(nthcdr 3 (nth inc_val ants) ))))
					;; Then remove the ant
					(remove (nth inc_val ants) ants)
					(format t "ant ascended to a higher point of existence!~%"  )
					;;Add to num_of_completed_paths - Our goal state is 30
					(setf num_of_completed_paths (+ num_of_completed_paths 1))
					;;Change the our no_check value so the loop can stop
					(setf no_check (+ no_check 1))
					
				)
		)
		;;Increments the value used to check each ant in the ants list
		(setf inc_val (+ inc_val 1))
		while (= no_check 0)
	)	
)

;;-------------------------------------------------- best_path ---

(defun best_path ()
	(setq best_length 0)
	(setq best_path_list '())
	
	(loop for i from 0 to (- (list-length completed_paths) 1) do 
		(if (= i 0) ;;base case
			(progn
			
			(setf best_path_list (nth 0 (nth i completed_paths)))
			(setf best_length (nth 1 (nth i completed_paths))))
		)
		(if (> best_length (nth 1 (nth i completed_paths)))
			(progn
			
			(setf best_path_list (nth 0 (nth i completed_paths)))
			(setf best_length (nth 1 (nth i completed_paths))))
		)	
	)
	(format t "~%best_length: ~a~%" best_length)
	(format t "~%best_path_list: ~a~%~%" best_path_list)	
	
	;;print completed path lengths
	(loop for i from 0 to (- (list-length completed_paths) 1) do
		(format t "ant ~a path length: ~a~%" i (nth 1 (nth i completed_paths)))
	)
)

;;-------------------------------------------------- example_1 ---
(defun example_1  ()
	
	(set_bar 1 8 1 16)
	(set_bar 24 0 0 15)
	(set_bar 25 45 0 15)
	(set_bar 7 15 1 18)
	(set_bar 18 10 0 7)
	(set_bar 14 51 0 9)
	(set_bar 20 36 1 4)
	(set_bar 14 13 1 1)
	(set_bar 11 14 0 16)
	(set_bar 22 37 1 7)
	(set_bar 9 17 0 19)
	(set_bar 22 9 0 18)
	(set_bar 30 3 1 10)
	(set_bar 15 20 0 6)
	(set_bar 15 28 0 20)
	(set_bar 34 23 0 18)
	(set_bar 35 41 1 2)
	(set_bar 25 0 0 13)
	(set_bar 11 39 1 7)
	(set_bar 9 20 1 10)
	(set_bar 1 54 0 0)
	(set_bar 3 15 0 27)
	(set_bar 5 0 0 11)
	(set_bar 17 36 1 11)
	(set_bar 18 41 0 12)
	(set_bar 0 43 1 19)
	(set_bar 5 12 0 17)
	(set_bar 0 24 1 16)
	(set_bar 18 23 0 12)
	(set_bar 0 43 1 19)
	(set_bar 5 12 0 17)
	(set_bar 0 24 1 16)
	(set_bar 18 23 0 12)
	(set_bar 38 30 1 2)
	(set_bar 17 12 1 8)
	
	
)
	
;;-------------------------------------------------- example_2 ---
(defun example_2  ()
	
	(set_bar 11 37 0 23)
	(set_bar 5 7 1 3)
	(set_bar 32 46 1 8)
	(set_bar 20 47 0 3)
	(set_bar 17 36 0 0)
	(set_bar 29 13 1 11)
	(set_bar 8 1 1 11)
	(set_bar 14 29 0 9)
	(set_bar 26 26 0 28)
	(set_bar 14 50 1 9)
	(set_bar 33 52 1 6)
	(set_bar 39 30 0 20)
	(set_bar 13 26 1 19)
	(set_bar 26 51 0 8)
	(set_bar 1 41 0 11)
	(set_bar 17 37 1 3)
	(set_bar 0 54 1 4)
	(set_bar 4 51 0 1)
	(set_bar 0 44 0 9)
	(set_bar 16 13 1 8)
	(set_bar 1 0 0 10)
	(set_bar 1 12 0 16)
	(set_bar 3 2 0 22)
	(set_bar 5 0 0 7)
	(set_bar 29 29 0 12)
	(set_bar 22 49 1 5)
	(set_bar 28 49 1 12)
	(set_bar 35 42 0 8)
	(set_bar 35 52 0 8)
	(set_bar 15 57 1 14)
	(set_bar 31 21 1 9)
	(set_bar 8 44 0 16)
	(set_bar 8 30 0 22)
	(set_bar 4 31 1 2)
	(set_bar 26 23 0 12)
	
	
)
;;-------------------------------------------------- example_3 ---
(defun example_3  ()
	
	(set_bar 11 37 0 23)
	(set_bar 5 7 1 3)
	(set_bar 32 46 1 8)
	(set_bar 20 47 0 3)
	(set_bar 17 36 0 0)
	(set_bar 29 13 1 11)
	(set_bar 8 1 1 11)
	(set_bar 14 29 0 9)
	(set_bar 26 26 0 28)
	(set_bar 14 50 1 9)
	(set_bar 33 52 1 6)
	(set_bar 39 30 0 20)
	(set_bar 13 26 1 19)
	(set_bar 26 51 0 8)
	(set_bar 0 22 1 5)
	(set_bar 1 41 0 11)
	(set_bar 17 37 1 3)
	(set_bar 0 54 1 4)
	(set_bar 4 51 0 1)
	(set_bar 0 44 0 9)
	(set_bar 16 13 1 8)
	(set_bar 1 0 0 10)
	(set_bar 1 12 0 16)
	(set_bar 3 2 0 22)
	(set_bar 5 0 0 7)
	(set_bar 29 29 0 12)
	(set_bar 22 49 1 5)
	(set_bar 28 49 1 12)
	(set_bar 35 42 0 8)
	(set_bar 35 52 0 8)
	(set_bar 15 57 1 14)
	(set_bar 31 21 1 9)
	(set_bar 8 44 0 16)
	(set_bar 8 30 0 22)
	(set_bar 26 23 0 12)
)

;;-------------------------------------------------- framework ---
(defvar grid '())
(defvar ants '())
(defvar completed_paths '())
(defvar num_of_completed_paths 0)

(defun framework ()
	;;initialize 
	(setf num_of_completed_paths 0)
	(setf ants '())
	(setf completed_paths '())
	(setf grid '())
	
	;;initialize (0,0) to (39,59) cells
	;;in (x y Hueristic scent isBar) form
	(setf grid (set_grid grid))
	
	;;set example case
	(example_2)	;;****change in order to get another example case*****
				;;****do not run two at the same time, might block all paths to goal****
	
	;;add single ant
	(setf ants (new_ant ants))
	
	(loop while (< num_of_completed_paths 5)	;; change value depending on number of completed_paths
		do (progn 
			;;loop through ants
			(loop for i from 0 to (- (list-length ants) 1) do
				;move ants
				(move_ant (nth i ants)))
				
			(loop for i from 0 to (- (list-length ants) 1) do
				;place scent
				(place_scent (nth 0 (nth i ants)) (nth 1 (nth i ants))))
			
			;;evaporate scent
			;(evaporate_scent)
			
			;;add ant after everyone has moved
			(setf ants (new_ant ants))))
	
	;;print best path and completed path lengths
	(best_path)
)
 
(framework)