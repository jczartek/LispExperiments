(defparameter *prefix-titles*
  '(Mr Mrs Miss Ms Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of a name")

(setf prefix-names '((John Q Public) (Malcom X)
                     (Admiral Grace Murray Hopper) (Spot)
                     (Aristotele) (A A Milne) (Z Z Top)
                     (Sir Larry Olivier) (Miss Scarlet)))

(defun first-name (name)
  "Select the first name from a name represented as a list"
  (if (member (first name) *prefix-titles*)
      (first-name (rest name))
      (first name)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *suffix-titles* '(MD Mr))
(setf suffix-names '((John Smith Mr) (Rex Morgan MD)
                     (Miss Scarlet) (John Len)))

(defun remove-last-item (lst)
  "Remove the last item with a list"
   (if (null (cdr lst))
       nil
       (cons (car lst) (remove-last-item (cdr lst)))))

(defun last-name (name)
  "Select the last name from a name represented as a list"
   (if (member (first (last name)) *suffix-titles*)
      (last-name (remove-last-item name))
      (first (last name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun power (x y)
  "Returns the value of x to the power of y"
  (if (zerop y)
       1
      (* x (power x (- y 1)))))

(defun power-opt (x n)
  "Power raises x to the nth power. N must be an integer >= 0.
  This executes in log n time, because of the check for even n."
  (cond ((= n 0) 1)
        ((evenp n) (expt (power x (/ n 2)) 2))
        (t (* x (power x (- n 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun count-atoms (lst)
  "Return the total  number of non-nil atoms in the expression."
  (cond ((null lst) 0)
        ((atom lst) 1)
        (t (+ (count-atoms (first lst))
              (count-atoms (rest lst))))))

(defun count-anywhere (item lst)
   "Count the times item appears anywhere within tree."
   (cond ((eql item lst) 1)
         ((atom lst) 0)
         (t (+ (count-anywhere item (first lst))
               (count-anywhere item (rest lst))))))

(defun count-all-atoms (lst &optional (if-null 1))
  "Return the total number of atoms in the expression, counting nil as an atom
  only in non-tail position."
  (cond ((null lst) if-null)
        ((atom lst) 1)
        (t (+ (count-all-atoms (first lst) 1)
              (count-all-atoms (rest lst) 0)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dot-product (1lst 2lst)
  (apply #'+ (mapcar #'* 1lst 2lst)))

(defun dot-product1 (1lst 2lst)
  (if (or (null 1lst) (null 2lst))
      0
      (+ (* (car 1lst) (car 2lst))
         (dot-product (cdr 1lst) (cdr 2lst)))))
