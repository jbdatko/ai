(defun make-node (env parent action cost)
  (list env parent action cost))

(defun solution (node)
  (if (null node)
      nil
      (list (third node) (solution (second node)))))

(defun nyc-distance (x y)
  "Returns the Manhattan distance"
  (+ (abs (- (car x) (car y)))
     (abs (- (cadr x) (cadr y)))))
