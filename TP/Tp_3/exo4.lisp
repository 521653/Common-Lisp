(defun eval_infix (L)
    (if (atom L) L
        (funcall (cadr L)(eval_infix (car L))(eval_infix (caddr L)))
    )
)