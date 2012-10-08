(defmacro while (test &rest body)
"While macro copied from ANSI Common LISP pg. 164"
  `(do ()
    ((not ,test))
    ,@body))

(defun make-node (env parent action cost)
  (list env parent action cost))

(defun solution (node)
  (if (null node)
      nil
      (let ((result (solution (cadr node))))
	(unless (eql nil result))
	(cons (caddr node) result))))

(defun nyc-distance (x y)
  "Returns the Manhattan distance"
  (+ (abs (- (car x) (car y)))
     (abs (- (cadr x) (cadr y)))))

(defclass explored-set()
  ((s :accessor e-set
      :initform (cons nil nil))))

(defmethod is-in-set (x (set explored-set))
  (member x (e-set set)))

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
  (or (equalp state GOAL-TOP-LEFT)
      (equalp state GOAL-BOTTOM-RIGHT)))

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

(defmethod is-in-q (x (q fifo-queue))
  (member x (queue-q q) :test #'equal))



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



(defun expand-node (current-node next-env frontier explored)
  (let ((child (make-node (car next-env) current-node (cadr next-env) '0)))
    (unless (or (is-in-q child frontier)
		(is-in-set (car child) explored))
    (enqueue child frontier))))

(defun bfs (frontier explored)
  (if (is-q-empty frontier)
      (nil)
      (let ((current (dequeue frontier)))
       (if (is-goal (car current))
	   (solution current)
	   (progn (add-to-set (car current) explored)
		  (mapcar #'(lambda (x) (expand-node current x frontier explored))
			  (successor (car current)))
		  (bfs frontier explored))))))

(defun ibfs (frontier explored)
  (if (is-q-empty frontier)
      (nil)
      (let ((current (dequeue frontier)))
       (if (is-goal (car current))
	   (solution current)
	   (progn (add-to-set (car current) explored)
		  (mapcar #'(lambda (x) (expand-node current x frontier explored))
			  (successor (car current)))
		  nil)))))



(defun search-bfs-print (puzzle)
  (let ((start (make-node puzzle nil nil nil))
	(frontier (make-instance 'fifo-queue))
	(explored (make-instance 'explored-set)))
    (enqueue start frontier)
    (print-puzzle (car (dequeue frontier)))))


(defun search-bfs (puzzle)
  (let ((start (make-node puzzle nil nil nil))
	(frontier (make-instance 'fifo-queue))
	(explored (make-instance 'explored-set)))
    (enqueue start frontier)
    (bfs frontier explored)))


(defun bfs1 (puzzle)
  (print-puzzle puzzle)
  (let ((start (make-node puzzle nil nil nil))
	(frontier (make-instance 'fifo-queue))
	(explored (make-instance 'explored-set)))
    (enqueue start frontier)
    (let ((sol (ibfs frontier explored)))
      (block search-loop
      	(while t
	  (if (eql nil sol)
	      (setf sol (ibfs frontier explored))
	      (return-from search-loop sol))))
      (cdr (reverse sol)))))


(defun bfs2 (puzzle)
  (let ((start (make-node puzzle nil nil nil))
	(frontier (make-instance 'fifo-queue))
	(explored (make-instance 'explored-set)))
    (enqueue start frontier)
    (let ((sol nil))
      (do ((i 0 (ibfs frontier explored)))
	  (unless (eql nil sol)
	    (setf sol (solution sol)))))))


;;test code

(setf set1 (make-instance 'explored-set))
(setf frontier1 (make-instance 'fifo-queue))
(setf start (make-node EASY1 nil nil nil))
