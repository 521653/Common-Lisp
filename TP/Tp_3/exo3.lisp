(defun SOM-VEC (V1 V2)
    (mapcar #'+ V1 v2)
)

(defun PRO-SCAL (V1 V2)
    (apply #'+ (mapcar #'* V1 V2))
)

(defun NORME-VEC (V)
    (sqrt (pro-scal v v))
)