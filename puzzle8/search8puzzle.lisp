;;Josh Datko <jbd65>
;;CS510 Assignment 1
;;15OCT12

;;The results queue is used for gather statistics
(defparameter results (make-empty-queue) "Queue in which results of runs are entered")

;;Constant definitions

(defconstant GOAL-TOP-LEFT (make-array '(3 3) :initial-contents '((9 1 2)(3 4 5)(6 7 8)))
	   "The goal state")


(defconstant GOAL-BOTTOM-RIGHT (make-array '(3 3) :initial-contents '((1 2 3)(4 5 6)(7 8 9)))
	   "The goal state")


(defconstant EASY1 (make-array '(3 3) :initial-contents '((1 9 2)(3 4 5)(6 7 8)))
	   "one move puzzle")


(defconstant EASY2 (make-array '(3 3) :initial-contents '((1 4 2)(3 9 5)(6 7 8)))
	   "two move puzzle")


(defconstant EASY3  (make-array '(3 3) :initial-contents '((1 4 2)(3 7 5)(6 9 8)))
	   "three move puzzle")

;;useful utility functions
(defun is-goal (state)
  "Returns true if the state equals the goal state"
  (or (equalp state GOAL-TOP-LEFT)
      (equalp state GOAL-BOTTOM-RIGHT)))

(defmacro while (test &rest body)
"While macro copied from ANSI Common LISP pg. 164"
  `(do ()
    ((not ,test))
    ,@body))

(defun make-node (env parent action cost)
  "Create an instance of a search node"
  (list env parent action cost))


(defun solution (node)
  "Given a node that equals the solution, return a list of moves that produces said solution"
  (cdr (reverse (get-solution node))))

(defun get-solution (node)
  "Recursively traverse the nodes parent to return a list of moves"
  (if (null node)
      nil
      (cons (caddr node) (get-solution (cadr node)))))

;;Heuristic function and helpers
(defun nyc-distance (x y)
  "Returns the Manhattan distance"
  (+ (abs (- (car x) (car y)))
     (abs (- (cdr x) (cdr y)))))


(defun h (state)
  "Heuristic function for 8-puzzle, essentially the sum of the manhattan distances.  Returns the sum."
  (let ((sum 0))
    (do ((x 0 (+ x 1)))
	((> x 2))
      (do ((y 0 (+ y 1)))
	  ((> y 2))
	(+ sum (nyc-distance (cons x y) (locate (aref state x y) GOAL-TOP-LEFT)))))
    (return-from h sum)))


(defun locate (num puzzle)
  "Utility function, it will locate the piece (number) in a puzzle and return an assoc list
   representing it's location"
  (do ((x 0 (+ x 1)))
      ((> x 2))
    (do ((y 0 (+ y 1)))
	((> y 2))
      (if (eql (aref puzzle x y) num)
	  (return-from locate (cons x y))
	  nil))))

;;The cost class and helper functions
;;
(defclass cost()
  ((g :accessor g
      :initarg :set-g
      :initform 0)))

(defmethod f ((c cost) state)
  "f equals g(node) + h(node)"
  (+ (g c) (h state)))

(defun get-f (node)
  "Given a node, return the f(node) = g(node) + h(node)"
  (f (cadddr node) (car node)))

(defun get-g (node)
  "Given a node, return g(node)"
  (g (cadddr node)))

;;End cost class

;;The explored set and functions
;;
(defclass explored-set()
  ((s :accessor e-set
      :initform (cons nil nil))))


(defmethod is-in-set (x (set explored-set))
  "If the state is in the set, return t otherwise nil"
  (member x (e-set set) :test #'equalp))

(defmethod add-to-set (x (set explored-set))
  "Add the state x to the set"
  (unless (is-in-set x set)
    (push x (e-set set))))

;;end explored-set

(defun successor (puzzle)
  "Return the successor states of the given puzzle state"
  (mapcar #'(lambda (x) (list (move puzzle x) x))
		    (valid-moves puzzle)))

(defun print-successor (puzzle)
  "Debug function to print the successor states"
  (mapcar #'(lambda (x) (print-puzzle (car x)))
	  (successor puzzle)))

;;
;;queue class and helpers
(defclass queue ()
  ((q :accessor queue-q
      :initform (cons nil nil))))

(defclass fifo-queue (queue)
  ())

(defclass lifo-queue (queue)
  ())

(defclass priority-queue (queue)
  ())


(defmethod is-q-empty ((q queue))
  (if (eql nil (car (queue-q q)))
      t
      nil))


(defmethod is-in-q (x (q queue))
  (if (is-q-empty q)
      nil
      (member x (queue-q q) :test #'equal)))

(defmethod dequeue ((q lifo-queue))
	   (pop (queue-q q)))

(defmethod enqueue (x (q lifo-queue))
	   (push x (queue-q q)))


(defmethod dequeue ((q fifo-queue))
  "FIFO Queue based on the Queue implementation in ANSI Common LISP pg. 201"
  (pop (car (queue-q q))))

(defmethod enqueue ( x (q fifo-queue))
  "FIFO Queue based on the Queue implementation in ANSI Common LISP pg. 201"
  (if (null (car (queue-q q)))
      (setf (cdr (queue-q q)) (setf (car (queue-q q)) (list x)))
      (setf (cddr (queue-q q)) (list x)
	    (cdr (queue-q q)) (cddr (queue-q q))))
  (car (queue-q q)))

(defmethod print-queue ( (q fifo-queue))
  (mapcar #'(lambda (x) (unless (eql nil x) (print-puzzle (car x))))
		    (queue-q q)))
;;end queue class and helpers


(defun make-child (current-node next-env)
  "Create a child instance from the current (parent)"
  (make-node (car next-env) current-node (cadr next-env)
	     (if (typep (cadddr current-node) 'cost)
		 (make-instance 'cost :set-g (+ 1 (get-g current-node))) ;;increase the cost for each child
		 '0)))


(defun exists-in (node frontier explored)
  "Membershp test for BFS/DFS frontiers and explored sets"
  (if (or (is-in-q node frontier)
	  (is-in-set (car node) explored))
      t
      nil))



(defun get-child (node)
  "Return children of this node (i.e. successors)"
  (mapcar #'(lambda (x) (make-child node x))
			 (successor (car node))))

(defun expand (node frontier explored)
  "Expand the node for BFS/DFS searchs and enqueue to frontier if not already there"
  (dolist (child (get-child node))
    (if (exists-in child frontier explored)
	nil
	(if (is-goal (car child))
	    (return-from expand (solution child))
	    (progn (setf *num-expanded* (+ *num-expanded* 1))
		   (enqueue child frontier)
		   nil)))))


;;Functions to gather the statistics
(defun build-solution (num algo node)
  "Get the solution and collect the results"
  (let ((sol (solution node)))
    (return-from build-solution (build-result-entry num algo sol))))

(defun build-result-entry (num algo sol)
  "Queue up the results of the node expansion data"
  (enqueue-at-front results (list (list num algo (length sol))))
  (return-from build-result-entry sol))


;;Main Search Routines

(defun solve-8puzzle (algo puzzle)
  "Main search entry point.  It will return a list of moves that solved the puzzle."
  (case algo
    ('BFS (graph-search puzzle (make-instance 'fifo-queue) 'BFS))
    ('DFS (graph-search puzzle (make-instance 'lifo-queue) 'DFS))
    ('ASTAR (astar puzzle))))

(defun graph-search (puzzle frontier algo)
  "Peform an uninformed search (i.e. BFS or DFS) on the puzzle using the passed in frontier.
   Depending on the type of queue, this method will either be BFS or DFS"
  (defparameter *num-expanded* 0) ;;for stats gathering
  (let ((node (make-node puzzle nil nil nil))
	(explored (make-instance 'explored-set)))
    (if (is-goal (car node))
	nil ;;no moves required, already at goal
	(progn
	  (enqueue node frontier)
	  (while (not (is-q-empty frontier)) ;;main search loop
	    (let ((current (dequeue frontier)))
	      (add-to-set (car current) explored)
	      (let ((sol (expand current frontier explored)))
		(if sol
		    (progn (build-result-entry *num-expanded* algo sol) ;;build the stats data
			   (return-from graph-search sol)) ;; return with the solution
		    sol))))))))

(defun astar (puzzle)
  "Perform informed A* search on the puzzle"
  (defparameter *num-expanded* 0)
  (let ((node (make-node puzzle nil nil (make-instance 'cost)))
	(explored (make-instance 'explored-set))
	(frontier (make-empty-queue)))
    (enqueue-by-priority frontier (list node) #'get-f)
    (while (not (empty-queue? frontier))
      (let ((node (remove-front frontier)))
	(if (is-goal (car node))
	    (return-from astar (build-solution *num-expanded* 'ASTAR node))
	    (progn
	      (add-to-set (car node) explored)
	      (dolist (child (get-child node))
		(if (is-in-set (car child) explored)
		    nil
		    (progn
		      (setf *num-expanded* (+ *num-expanded* 1))
		      (enqueue-by-priority frontier (list child) #'get-f))))))))))

;;
;;The following is test code and functions to gather and print statistics
;;
(defun test-verify (puzzle)
  (is-goal (last (mapcar #'(lambda (x) (move puzzle x))
			 (bfs1 puzzle)))))

(defun verify (puzzle fn)
  (let ((sol (funcall fn puzzle)))
    (dolist (direction sol)
      (setf puzzle (move puzzle direction)))
    (print-puzzle puzzle)))


(setf s (make-instance 'explored-set))
(setf f (make-instance 'lifo-queue))
(setf start (make-node EASY3 nil nil (make-instance 'cost)))

(defun print-results (q)
  (if (empty-queue? q)
      nil
      (progn (format t "~A ~A ~A~%" (car (queue-front q)) (cadr (queue-front q)) (caddr (queue-front q)))
	     (remove-front q)
	     (print-results q))))


(defun test-solver (algo iterations)
  "run a search algorithm iterations times"
  (if (eql 0 iterations)
      0
      (progn (solve-8puzzle algo (random-puzzle))
	     (test-solver algo (- iterations 1)))))

(defun print-node (node)
  "Print the node in human readable format"
  (format t "State: ~A~%Parent: ~A~%Action: ~A~%Cost: ~A~%"
	  (car node) (cadr node) (caddr node) (cadddr node)))
