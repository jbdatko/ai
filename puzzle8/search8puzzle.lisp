(defmacro while (test &rest body)
"While macro copied from ANSI Common LISP pg. 164"
  `(do ()
    ((not ,test))
    ,@body))

(defun make-node (env parent action cost)
  (list env parent action cost))

;; (defun solution (node)
;;   (if (null node)
;;       nil
;;       (let ((result (solution (cadr node))))
;; 	(unless (eql nil result))
;; 	(cons (caddr node) result))))

(defun solution (node)
  (cdr (reverse (get-solution node))))

(defun get-solution (node)
  (if (null node)
      nil
      (cons (caddr node) (get-solution (cadr node)))))

(defun nyc-distance (x y)
  "Returns the Manhattan distance"
  (+ (abs (- (car x) (car y)))
     (abs (- (cdr x) (cdr y)))))

(defun h (state)
  (let ((sum 0))
    (do ((x 0 (+ x 1)))
	((> x 2))
      (do ((y 0 (+ y 1)))
	  ((> y 2))
	(setf sum (+ sum (nyc-distance (cons x y) (locate (aref state x y) GOAL-TOP-LEFT))))))
    (return-from h sum)))


(defun locate (num puzzle)
  (do ((x 0 (+ x 1)))
      ((> x 2))
    (do ((y 0 (+ y 1)))
	((> y 2))
      (if (eql (aref puzzle x y) num)
	  (return-from locate (cons x y))
	  nil))))


(defclass cost()
  ((g :accessor g
      :initarg :set-g
      :initform 0)))

(defmethod f ((c cost) state)
  (+ (g c) (h state)))

(defmethod increment-cost ((c cost))
  (setf (g c) (+ (g c) 1)))

(defun get-f (node)
  (f (cadddr node) (car node)))

(defun get-g (node)
  (g (cadddr node)))

(defclass explored-set()
  ((s :accessor e-set
      :initform (cons nil nil))))


(defmethod is-in-set (x (set explored-set))
  (member x (e-set set) :test #'equalp))

(defmethod add-to-set (x (set explored-set))
  (unless (is-in-set x set)
    (push x (e-set set))))

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

(defun is-goal (state)
  "Returns true if the state equals the goal state"
  (equalp state GOAL-TOP-LEFT))
;      (equalp state GOAL-BOTTOM-RIGHT)))

(defun successor (puzzle)
  (mapcar #'(lambda (x) (list (move puzzle x) x))
		    (valid-moves puzzle)))

(defun print-successor (puzzle)
  (mapcar #'(lambda (x) (print-puzzle (car x)))
	  (successor puzzle)))

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


(defun make-child (current-node next-env)
  (make-node (car next-env) current-node (cadr next-env)
	     (if (typep (cadddr current-node) 'cost)
		 (make-instance 'cost :set-g (+ 1 (get-g current-node)))
		 '0)))


(defun expand-node (current-node next-env frontier explored)
  (let ((child (make-node (car next-env) current-node (cadr next-env) '0)))
    (unless (or (is-in-q child frontier)
		(is-in-set (car child) explored))
    (enqueue child frontier))))




(defun test-verify (puzzle)
  (is-goal (last (mapcar #'(lambda (x) (move puzzle x))
			 (bfs1 puzzle)))))

(defun verify (puzzle fn)
  (let ((sol (funcall fn puzzle)))
    (dolist (direction sol)
      (setf puzzle (move puzzle direction)))
    (print-puzzle puzzle)))

;; (defun bfs2 (puzzle)
;;   (let ((start (make-node puzzle nil nil nil))
;; 	(frontier (make-instance 'fifo-queue))
;; 	(explored (make-instance 'explored-set)))
;;     (enqueue start frontier)
;;     (let ((sol nil))
;;       (do ((i 0 (ibfs frontier explored)))
;; 	  (unless (eql nil sol)
;; 	    (setf sol (solution sol)))))))


;;test code

(setf s (make-instance 'explored-set))
(setf f (make-instance 'lifo-queue))
(setf start (make-node EASY3 nil nil (make-instance 'cost)))

(defun exists-in (node frontier explored)
  (if (or (is-in-q node frontier)
	  (is-in-set (car node) explored))
      t
      nil))


(defun print-node (node)
  (format t "State: ~A~%Parent: ~A~%Action: ~A~%Cost: ~A~%"
	  (car node) (cadr node) (caddr node) (cadddr node)))

(defun get-child (node)
  (mapcar #'(lambda (x) (make-child node x))
			 (successor (car node))))

(defun expand (node frontier explored)
  (dolist (child (get-child node))
    (if (exists-in child frontier explored)
	nil
	(if (is-goal (car child))
	    (return-from expand (cdr (reverse (solution child))))
	    (progn (enqueue child frontier)
		   nil)))))

(defun bfs3 (puzzle frontier)
  (print-puzzle puzzle)
  (let ((node (make-node puzzle nil nil (make-instance 'cost)))
	(explored (make-instance 'explored-set)))
    (if (is-goal (car node))
	nil
	(progn
	  (enqueue node frontier)
	  (while (not (is-q-empty frontier))
	    (let ((current (dequeue frontier)))
	      (add-to-set (car current) explored)
	      (let ((sol (expand current frontier explored)))
		(if sol
		    (return-from bfs3 sol)
		    sol))))))))

(defun run-test (num algo)
  (if (eql 'BFS algo)
      (bfs3 (random-puzzle num) (make-instance 'fifo-queue))
      (bfs3 (random-puzzle num) (make-instance 'lifo-queue))))


(defun astar (puzzle)
  (print-puzzle puzzle)
  (let ((node (make-node puzzle nil nil (make-instance 'cost)))
	(explored (make-instance 'explored-set))
	(frontier (make-empty-queue)))
    (enqueue-by-priority frontier (list node) #'get-f)
    (while (not (empty-queue? frontier))
      (let ((node (remove-front frontier)))
	(if (is-goal (car node))
	    (return-from astar (solution node))
	    (progn
	      (add-to-set (car node) explored)
	      (dolist (child (get-child node))
		(if (is-in-set (car child) explored)
		    nil
		    (enqueue-by-priority frontier (list child) #'get-f)))))))))
