(defun make-node (env parent action cost)
  (list env parent action cost))

(defun solution (node)
  (if (null node)
      nil
      (list (caddr node) (solution (cadr node)))))

(defun nyc-distance (x y)
  "Returns the Manhattan distance"
  (+ (abs (- (car x) (car y)))
     (abs (- (cadr x) (cadr y)))))


(defun is-in-set (x set)
  (member x set))

(defun add-to-set (x set)
  (unless (is-in-set x set)
    (push x set)))

(defconstant GOAL (make-array '(3 3) :initial-contents '((9 1 2)(3 4 5)(6 7 8)))
	   "The goal state")

(defun is-goal (state)
	   "Returns true if the state equals the goal state"
	   (equal state GOAL))

(defun successor (puzzle)
  (mapcar #'(lambda (x) (move puzzle x)) (valid-moves puzzle)))

(defun print-successor (puzzle)
	   (mapcar #'(lambda (x) (print-puzzle x)) (successor puzzle)))
