;Author       : Steven Espin, Natalie Ang, Marc Jimenez
;Contact Info :	sespin@csu.fullerton.edu
;team         :	snm
;description  :	Genetic Program to find fitness of arithmetic expressions over generational time.				
;				Alphabet: Variables X, Y, Z; Integers -9, 8, ..., 8, 9; Operators +, -, *

;cell-count (rt)
 "Return the number of nodes/cells in the tree. Skip non-cells."
;tree-nth (rnth rtree)
 "Return the DFS N-th subtree/elt in the given tree."
;tree-nth-cell (rnth rtree)
 "Return the DFS N-th cell in the given tree: 1-based."
;random-tree-cell (rtree)
 "Return random cell in the tree, but not the whole tree."
;newop-random-tree-cell (rtree)
 "Return random cell in the tree, must start with operator."
;newnop-random-tree-cell (rtree)
 "Return random cell in the tree, must start with non operator."
;make-kid (rmom rtgt rnew)
 "Return kid: copy of mom with tgt cell replaced by given new cell, or nil."
;test-make-kid (rtree)
 "Test make-kid with random tgt cell and fixed replacement list."
;look-for-operator (rt)
 "Look through a tree to see whether an operator is found."
;cross-parents (par1 par2)
 "given two parents, return a child."
;mutation (rt)
 "look through every leaf node and apply 1% chance of same type mutation"
;get-front-upto-nth ( rn rlist )
 "Return list head from 0-th thru N-th elt."
;get-score (rcritter)
 "Get score for critter."
;score-pop ( rpop )
 "Create Pop-Scored pairs, like (Score Critter), given Pop list of critters."
;safe-sort-scored-pop ( rscored-pop )
 "Return a sorted list of scored-critter pairs. Don't change given list."
;get-pop-from-scored (rscored-pop)
 "Return just the Pop of critters from the Scored Pop."
;random-expression-no-sublist ()
 "return random expression with no possibility of having sub-expression"
;random-expression ()
 "return random expression"
;framework () 
 "framework for generating 50 generations of critters"

 

(setf *random-state* (make-random-state t)) ;;;for random states
(defvar ops '(+ - *)) ;;;operators allowed
(defvar nonops '(x y z -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9)) ;;;non operators allowed
 
;; ---------------------------------------------------------- cell-count ----
(defun cell-count (rt)
 "Return the number of nodes/cells in the tree. Skip non-cells."
 (cond
	((null rt) 0) ;;; if node is null, return 0
	((not (listp rt)) 0) ;;; if not a list, return 0
	(t (let ((cc (length rt)))	;;; else do this 
	   (+ cc (apply #'+ (mapcar #'cell-count rt)))))))
 
;; Tests
;;(cell-count '(a b c))
;;3
;;(cell-count '(a (b) c))
;;4
;; (cell-count '(a))
;; 1
;; (cell-count '(+ x (* 2 (+ z 7) z) 3 4))
;; 12

;; ------------------------------------------------------------ tree-nth ----
(defun tree-nth (rnth rtree)
 "Return the DFS N-th subtree/elt in the given tree." ;;gives subtree or element itself if no subtree
													  ;;return nth car
 (let ((size (cell-count rtree)))
	;; (print (list :dbga rnth size (car rtree)))
	(cond
		((not (integerp rnth)) nil)
		((not (listp rtree)) nil) ;; Not a tree?
		((null rtree) nil) ;; No tree elts?
		((= 1 rnth) (car rtree)) ;; 1st elt of list is its car subtree.
		((>= 0 rnth) nil) ;; Nth 0 or negative?
		((> rnth size) nil) ;; N-th beyond tree's end?
		((= 1 size) (car rtree)) ;; 1st elt is Tree's car.
		(t ;; Elt is in car subtree or cdr "subtree".
			(setq rnth (1- rnth)) ;;Account: Elt isn't the current (car+cdr's) node.
			(let ((size1 (cell-count (car rtree))))	;;size of list in car if any
				;; (print (list :dbgb rnth size1 (car rtree)))
				
				;;checking whether to go down deeper in tree (is list in car) or keep going through cdr
				(cond
					((>= 0 size1) (tree-nth ;; No car subtree.
						rnth
						(cdr rtree))) ;; Find elt in the cdr subtree.
					((<= rnth size1) (tree-nth ;; Elt is in car subtree.
						rnth
						(car rtree))) ;; Find elt in the car subtree.
					(t (tree-nth ;; Elt is in cdr subtree.
						(- rnth size1) ;; Account for skipping car subtree.
						(cdr rtree))))))))) ;; Skip car subtree.
;; Tests
;; (tree-nth 1.3 '(a))
;; nil
;; (tree-nth 1 nil)
;; nil
;; (tree-nth 1 'a)
;; nil
;; (tree-nth 1 '(a))
;; a
;; (tree-nth 1 '(a b c))
;; a
;; (tree-nth 1 '((a) b c))
;; (a)
;; (tree-nth 2 '(a b c))
;; b
;; (tree-nth 2 '(a (b) c))
;; (b)
;; (tree-nth 3 '(a b c))
;; c
;; (tree-nth 4 '(a b c))
;; nil
;; (tree-nth 3 '(a (b) c))
;; b
;; (tree-nth 3 '((a f) b c))
;; f
;; (tree-nth 4 '((a f) b c))
;; b
;; (tree-nth 5 '((a f) b c))
;; c
;; (tree-nth 6 '((a f) b c))
;; nil
;; (tree-nth 6 '((a f) b c))
;; nil
;; (tree-nth 6 '((a f) ((b)) c))
;; b
;; (tree-nth 7 '((a f) ((b)) c))
;; c
;; (tree-nth 4 '(+ x (* 2 (+ z 7) z) 3 4))
;; *

;; ------------------------------------------------------- tree-nth-cell ----
(defun tree-nth-cell (rnth rtree)
 "Return the DFS N-th cell in the given tree: 1-based." ;;gives rest of list that is on same level as itself
 (let ((size (cell-count rtree)))
	;;(print (list :dbga rnth size (car rtree)))
	(cond
		((not (integerp rnth)) nil)
		((not (listp rtree)) nil) ;; Not a tree?
		((null rtree) nil) ;; No tree elts?
		((= 1 rnth) rtree) ;; 1st elt of list is the tree, itself.
		((>= 0 rnth) nil) ;; Nth 0 or negative?
		((> rnth size) nil) ;; N-th beyond tree's end?
		(t ;; Elt is in car subtree or cdr "subtree".
			(setq rnth (1- rnth)) ;;Account: Elt isn't the current (car+cdr's) node.
			(let ((size1 (cell-count (car rtree))))
				;;(print (list :dbgb rnth size1 (car rtree)))
				(cond
					((>= 0 size1) (tree-nth-cell ;; No car subtree.
						rnth
						(cdr rtree))) ;; Find elt in the cdr subtree.
					((<= rnth size1) (tree-nth-cell ;; Elt is in car subtree.
						rnth
						(car rtree))) ;; Find elt in the car subtree.
					(t (tree-nth-cell ;; Elt is in cdr subtree.
						(- rnth size1) ;; Account for skipping car subtree.
						(cdr rtree))))))))) ;; Skip car subtree.
;; Tests
;; (tree-nth-cell 1 nil)
;; nil
;; (tree-nth-cell 1 '(a b c))
;; (a b c)
;; (tree-nth-cell 2 '(a b c))
;; (b c)
;; (tree-nth-cell 3 '(a b c))
;; (c)
;; (tree-nth-cell 1 '((a b) c))
;; ((a b) c)
;; (tree-nth-cell 2 '((a b) c))
;; (a b)
;; (tree-nth-cell 3 '((a b) c))
;; (b)
;; (tree-nth-cell 4 '((a b) c))
;; (c)
;; (tree-nth-cell 5 '((a b) c))
;; nil
;; (tree-nth-cell 2 '((a f) ((b)) c))
;; (a f)
;; (tree-nth-cell 3 '((a f) ((b)) c))
;; (f)
;; (tree-nth-cell 4 '((a f) ((b)) c))
;; (((b)) c)
;; (tree-nth-cell 5 '((a f) ((b)) c))
;; ((b))
;; (tree-nth-cell 6 '((a f) ((b)) c))
;; (b)
;; (tree-nth-cell 7 '((a f) ((b)) c))
;; (c)
;; (tree-nth-cell 8 '((a f) ((b)) c))
;; nil
;; (tree-nth-cell 4 '(+ x (* 2 (+ z 7) z) 3 4))
;; (* 2 (+ z 7) z)


;; ---------------------------------------------------- random-tree-cell ----
(defun random-tree-cell (rtree)
 "Return random cell in the tree, but not the whole tree."
 (let* ((size (cell-count rtree))
	(rx (1+ (random (1- size)))) ;; Avoid 1st cell (the whole tree).
	(nth (1+ rx)) ;; Incr cuz our fcn is 1-based, not 0-based.
	(spot (tree-nth-cell nth rtree)))
	;; (print (list :dbg size nth spot))
	spot))
 
;; Tests
;; (random-tree-cell '(+ (* 5 a b) (* c (- d 6))))
;; ((* c (- d 6))) ;; No op
;; (- d 6) ;; Has op
;; ((* 5 a b) (* c (- d 6))) ;; No op
;; (* 5 a b) ;; Has op
;; (+ (* 5 a b) (* c (- d 6))) ;; Has op
;; (* c (- d 6)) ;; Has op

;; ---------------------------------------------------- newop-random-tree-cell ----
(defun newop-random-tree-cell (rtree)
 "Return random cell in the tree, must start with operator."
 (let* ((size (cell-count rtree))
	(rx (1+ (random (1- size)))) ;; Avoid 1st cell (the whole tree).
	(nth (1+ rx)) ;; Incr cuz our fcn is 1-based, not 0-based.
	(spot (tree-nth-cell nth rtree)))
	(print rtree)
	;; (print (list :dbg size nth spot))
	(if (member (car spot) ops) spot (newop-random-tree-cell rtree))))

;; Tests
;; (newop-random-tree-cell '(+ (* 5 a b) (* c (- d 6))))
;; (* C (- D 6))
;; (newop-random-tree-cell '(+ (* 5 a b) (* c (- d 6))))
;; (- D 6)
	
;; ---------------------------------------------------- newnop-random-tree-cell ----
(defun newnop-random-tree-cell (rtree)
 "Return random cell in the tree, must start with non operator."
 (let* ((size (cell-count rtree))
	(rx (1+ (random (1- size)))) ;; Avoid 1st cell (the whole tree).
	(nth (1+ rx)) ;; Incr cuz our fcn is 1-based, not 0-based.
	(spot (tree-nth-cell nth rtree)))
	;; (print (list :dbg size nth spot))
	(if (member (car spot) ops) (newnop-random-tree-cell rtree) spot)))

;; Tests
;; (newnop-random-tree-cell '(+ (* 5 a b) (* c (- d 6))))
;; (B)
;; (newnop-random-tree-cell '(+ (* 5 a b) (* c (- d 6))))
;; ((- D 6))
;; (newnop-random-tree-cell '(+ (* 5 a b) (* c (- d 6))))
;; ((* 5 A B) (* C (- D 6)))

;; ------------------------------------------------------------ make-kid ----
(defun make-kid (rmom rtgt rnew)
 "Return kid: copy of mom with tgt cell replaced by given new cell, or nil."
 (if (not (and rmom rtgt rnew
	(listp rmom)
	(listp rtgt)
	(listp rnew)))
	
	rmom
	
	(if (eq rmom rtgt)
		rnew
		(cons (make-kid (car rmom) rtgt rnew)
			(make-kid (cdr rmom) rtgt rnew)))))
 
 ;; ------------------------------------------------------- test-make-kid ----
(defun test-make-kid (rtree)
 "Test make-kid with random tgt cell and fixed replacement list."
 (let* ((tgt (random-tree-cell rtree))
	(newop '(+ 2 3)) ;; New has op.
	(newnop '(8 9))) ;; New has no op.
	(print (list :dbg :tgt tgt))
	(make-kid rtree
		tgt
		(if (member (car tgt) ops) ;; Tgt also has an op?
			newop
			newnop))))
			
;; Tests
;; (cell-count '(+ (* 5 a b) (* c (- d 6))))
;; 13
;; (test-make-kid '(+ (* 5 a b) (* c (- d 6))))
;; (:dbg :tgt (c (- d 6)))
;; (+ (* 5 a b) (* 8 9))
;; (:dbg :tgt ((- d 6)))
;; (+ (* 5 a b) (* c 8 9))
;; (:dbg :tgt ((- d 6)))
;; (+ (* 5 a b) (* c 8 9))
;; (:dbg :tgt (d 6))
;; (+ (* 5 a b) (* c (- 8 9)))
;; (:dbg :tgt (5 a b))
;; (+ (* 8 9) (* c (- d 6)))
;; (:dbg :tgt (a b))
;; (+ (* 5 8 9) (* c (- d 6)))
;; (:dbg :tgt ((* 5 a b) (* c (- d 6))))
;; (+ 8 9)
;; (:dbg :tgt ((- d 6)))
;; (+ (* 5 a b) (* c 8 9))
;; (:dbg :tgt (6))
;; (+ (* 5 a b) (* c (- d 8 9)))

;; -------------------------------------------------- look-for-operator --
(setf opFound 0) 

(defun look-for-operator (rt)
 "Look through a tree to see whether an operator is found."
 (cond
	((null rt) 0) ;;; if node is null, return 0
	((not (listp rt)) 0) ;;; if not a list, return 0
	((member (car rt) ops) (setf opFound 1)) ;; if element is an operator
	(t 	;;; else continue looking through tree
	   (mapcar #'look-for-operator rt))))

;; (look-for-operator '(2 4 (+ 4 5) 6))
;; (print opFound)
;; 1

;; -------------------------------------------------- cross-parents ------

(defun cross-parents (par1 par2)
 "given two parents, return a child."
 (let* ((tgt (random-tree-cell par1))
	(opFound 0))
	;; get random section from parent 2 to insert into parent 1
	(look-for-operator par2)
	(if (= opFound 1) (setf newop (newop-random-tree-cell par2)) (setf newop '())) ;; New has op.
	(setf newnop (newnop-random-tree-cell par2)) ;; New has no op.
	;;(print (list :dbg :tgt tgt))
	(make-kid par1
		tgt
		(if (member (car tgt) ops) ;; Tgt also has an op?
			newop
			newnop))))
;; (cross-parents '(+ 1 2 (+ 10 10)) '(+ 2 (+ 4 8) 5))
;; (+ 1 2 4 8)
;; (cross-parents '(+ 1 2 (+ 10 10)) '(+ 2 (+ 4 8) 5))
;; (+ 1 2 (+ 10 2 (+ 4 8) 5))

;; -------------------------------------------------- mutation ----

(defvar global (list )) ;;;to be populated later
(defvar mutation-count 0)

(defun mutation (rt)
 "look through every leaf node and apply 1% chance of same type mutation"
 (if (member rt ops) ;;; if element is operator, chance to mutate
	(if (<= (random 100) 1) ;;;1% chance
		(progn
		(setf global (subst (nth (random 3) ops) rt global))
		(setf mutation-count (+ mutation-count 1)))))	;;;mutate

 (if (member rt nonops) ;;; if element is non operator, chance to mutate
	(if (<= (random 100) 1) ;;;1% chance
		(progn
		(setf global (subst (nth (random 22) nonops) rt global))
		(setf mutation-count (+ mutation-count 1))))) ;;;mutate
 
 (cond
	((null rt)) ;;; if node is null, do nothing
	((not (listp rt))) ;;; if not a list, do nothing
	(t (mapcar #'mutation rt)))) ;;; keep going

;mutation makes use of global to make changes 1% of times too original tree,
;then sets original back to global with changes in place

;original tree
;(print tree)
;(+ 3 5)

;(setf global tree)
;(mutation tree)		
;(setf tree global)
;(print tree)
;(+ 3 5) 	no mutation

;; -------------------------------------------------- get-front-upto-nth ----
(defun get-front-upto-nth ( rn rlist )
 "Return list head from 0-th thru N-th elt. Assumes elt-n is unique."
 (let ((elt-n (nth rn rlist)))
	(reverse (member elt-n (reverse rlist)))))
	
;; Tests
;; (setq my-list '((1 a) (2 b) (3 c) (4 d) (5 e) (6 f) (7 g)))
;; (get-front-from-nth 4 my-list)
;; ((1 a) (2 b) (3 c) (4 d) (5 e))
;; (get-front-from-nth 2 my-list)
;; ((1 a) (2 b) (3 c))

;; ---------------------------------------------------------- get-score ----
(defun get-score (rcritter)
 "Get score for critter."
	(setf score (eval rcritter))
	
	(setf distance-from-goal-value (- score goal-value))
	(setf distance-from-goal-value (abs distance-from-goal-value))
	
	distance-from-goal-value)
;; tests -- depends on distance from goal-value, when solving for x y and z in expression
;; (x y z goal-score -> 0 -2 1 -16)
;; (get-score '(+ (- -4 7) -5 X))
;; 0
;; ---------------------------------------------------------- score-pop ----
(defun score-pop ( rpop ) ;; Pop is a population.
 "Create Pop-Scored pairs (Score Critter) given Pop list of critters."
 (mapcar #'(lambda (critter)
	(let ((score (get-score critter)))
		(list score critter)))
	rpop))
;; Tests
;; (setq my-pop '((a b c)
;; (a)
;; (e f g)
;; (a d)))
;; ((a b c) (a) (e f g) (a d))
;; (setq my-pop-scored (score-pop my-pop))
;; ((3 (a b c)) (1 (a)) (3 (e f g)) (2 (a d)))


;; ------------------------------------------------ safe-sort-scored-pop ----
(defun safe-sort-scored-pop ( rscored-pop )
 "Return a sorted list of scored-critter elts. Don't change given list.
 NB, your Lisp's built-in sort fcn may damage the incoming list."
 (let ((sacrifice-list (copy-list rscored-pop)))
	(sort sacrifice-list
		#'(lambda (scored-critter-1 scored-critter-2)
			(< (car scored-critter-1) (car scored-critter-2))))))
;; Tests
;; my-pop-scored
;; ((3 (a b c)) (1 (a)) (3 (e f g)) (2 (a d)))
;; (safe-sort-scored-pop my-pop-scored)
;; ((1 (a)) (2 (a d)) (3 (a b c)) (3 (e f g)))
;; my-pop-scored
;; ((3 (a b c)) (1 (a)) (3 (e f g)) (2 (a d)))

;; ------------------------------------------------- get-pop-from-scored ----
(defun get-pop-from-scored (rscored-pop)
 "Return just the Pop of critters from the Scored Pop."
 ;;Alt: (mapcar #'(lambda (elt) (nth 1 elt)) rscored-pop)
 (mapcar #'cadr rscored-pop))
 
;; Tests
;; my-pop-scored
;; ((3 (a b c)) (1 (a)) (3 (e f g)) (2 (a d)))
;; (get-pop-from-scored my-pop-scored)
;; ((a b c) (a) (e f g) (a d))

;; ------------------------------------------------- random-expression-no-sublist ----
(defun random-expression-no-sublist ()
	"return random expression with no possibility of having sub-expression"
	(setf expression '())
	
	(setf expression (append expression (list (nth (random 3) ops))))	;; set first element to (+, -, *)
	(setf expression (append expression (list (nth (random 22) nonops))))	;;set second element to (x, y, z) or (-9, -8....8, 9) or new random expression	
	(setf expression (append expression (list (nth (random 22) nonops))))	;;set third element to (x, y, z) or (-9, -8....8, 9) or new random expression
			
	expression)	;;return randomized expression 
	
;; Tests
;; (random-expression-no-sublist)
;; (- 2 9)
;; (random-expression-no-sublist)
;; (- -6 -6)

;; ------------------------------------------------- random-expression ----


(defun random-expression ()
	"return random expression"
	(setf expression '())
	
	(setf expression (append expression (list (nth (random 3) ops))))	;; set first element to (+, -, *)
	(if (= (random 3) 0) (setf expression (append expression (list (random-expression-no-sublist))))) ;; chance to add extra expression
	(setf expression (append expression (list (nth (random 22) nonops))))	;;set second element to (x, y, z) or (-9, -8....8, 9) or new random expression	
	(if (= (random 3) 0) (setf expression (append expression (list (random-expression-no-sublist))))) ;; chance to add extra expression
	(setf expression (append expression (list (nth (random 22) nonops))))	;;set third element to (x, y, z) or (-9, -8....8, 9) or new random expression
	(if (= (random 3) 0) (setf expression (append expression (list (random-expression-no-sublist))))) ;; chance to add extra expression
	expression)	;;return randomized expression 

;; Tests
;; (random-expression)
;; (- (- -4 7) Y (- -2 7) 7 (- 8 -1))
;; (random-expression)
;; (+ (+ Y -2) 2 -2)

;; ------------------------------------------------- set-xyz-goal ----	

(defvar x 0)
(defvar y -2)
(defvar z 1)
(defvar goal-value -16)

(defun set-xyz-goal (new-x new-y new-z new-goal)
	"helper function to set global variables x, y, z, and goal-value"
	(setf x new-x)
	(setf y new-y)
	(setf z new-z)
	(setf goal-value new-goal))
	
;; ------------------------------------------------- GP framework ----

(defun framework () 
	"framework for generating 50 generations of critters"
	(setf mutation-count 0)
	(let ((population (list )))
		;;make initial population - 50 total
		(loop for i from 0 to 49 do
			(setf population (append population (list (random-expression)))))
			
		(format t "~%initial population: ~a~%~%" population)
			
		;;get ready to output generation data table
		(format t "~%(x y z goal-fitness) -> (~a ~a ~a ~a)~%~%" x y z goal-value)
		(format t "            most fit            ~%")
		
		(loop for generation-count from 0 to 49 do ;;run 50 generations
			;;calculate fitness of current generation
			(setf scored-population (score-pop population))
			(setf scored-population (safe-sort-scored-pop scored-population))
			
			;;output generations fittest
			(format t "gen ~a:   ~a = ~a       ~%" generation-count (nth 1 (nth 0 scored-population)) 
				(nth 0 (nth 0 scored-population)) )
			
			;;pick 2 random parents from 25 fittest and cross to make a child
			;;repeat process 50 times to get 50 total next-population 
			(setf next-population (list))
			(loop for mating-count from 0 to 49 do
					;;pick two parents
					(setf parent1 (nth 1 (nth (random 25) scored-population)))
					(setf parent2 (nth 1 (nth (random 25) scored-population)))
					;;create two children based on two parents 
					(setf child (list (cross-parents parent1 parent2)))
					;;put through radiation/mutation
					(setf global child)
					(mutation child)	;;;1% chance
					(setf child global)
					;;add children to next-population
					(setf next-population (append next-population child))
			)
			
			;;make next-population of children = population for next loop 
			(setf population next-population)
		)
		(format t "~%mutations count: ~a~%~%" mutation-count)))

(framework) ;; call once on start