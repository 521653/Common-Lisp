(defun mystere(x y)
    (or(and (zerop x)(zerop y))
        (and(< x 0)(< y 0))
        (and(> x 0)(> y 0))))

(defun boulangerie (j h)
    (if(or
            (and (> j 1)(>= h 16)(<= h 20))
            (and(/= j 1)(>= h 16)(<= h 20)))
            "La boulangerie est ouverte" "La boulangerie n'est pas ouverte")

)       

(defun bis(a)
    (or(= (mod a 400) 0)
        (and (= (mod a 4) 0)(/= (mod a 100) 0))
)    
)

;Exercice5
 (defun triangle( x y z)

         (or(> (+ y z) x) (> (+ x z) y)(> (+ x y) z)))

(defun triangle_rectangle(x y z)

       (let ((hypotenuse (max x y z)))

              (= (expt hypotenuse 2) (or(+ (expt x 2) (expt  y 2))(+ (expt z 2)(expt x 2))(+ (expt z 2)(expt y 2))))))

 

(defun triangle_isocele( x y z)

       (if( triangle x y z)  (if (or(= x y)(= y z)(= x z)) T NIL) NIL))

 

(defun triangle_equilateral( x y z)

         (if( triangle x y z) (if (= x y z )  T NIL)  NIL))

 