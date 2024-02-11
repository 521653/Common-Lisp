(defun valeurAbs(L)
    (mapcar #'abs L)
)

(defun paire1(L)
    (remove-if-not #'evenp L)
)

(defun paire2(L)
    (remove-if-not (lambda(x)(zerop (mod x 2))) L)
)

(defun divisibleN(L n)
    (remove-if (lambda(x)(/= (mod x n) 0)) L)
)