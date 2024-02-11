(defun fusion(L1 L2)
  (cond 
    ((null L1) L2)
    ((<= (car L1) (car L2)) (cons (car L1) (fusion (cdr L1) L2)))
    (t (cons (car L2) (fusion L1 (cdr L2))))
  )
)

;Tp_4 exo1:
;;1
(defun longueur(L)
  (if (null L)
    0
    (+ 1 (longueur (cdr L)))
  )
)
;;2
(defun somme (L)
  (if (null L )
    0
    (+ (car L) (somme (cdr L)))
  )
)
;;3
(defun somme2 (L)
  (if(null L)
    0
    (if(numberp (car L)) (+ (car L) (somme2 (cdr L))) (somme2 (cdr L)))
  )
)

;exo2:
(defun countless (N L)
  (if(null L)
    0
    (+ (if(< (car L) N) 1 0) (countless N (cdr L)))
  )
)

;exo3:
;;1
(defun inverse (L)
  (if(null L)
    NIL
    (append (inverse (cdr L)) (list (car L)))
  )
)
;;2
(defun triee (L)
  (if (null (cadr L))
      t
      (if(< (car L) (cadr L)) NIL
           (triee (cdr L))
      )
  )
)

;exo4
;;1
(defun appartient (N L)
  (if(null L)
    NIL
    (if(eq N (car L)) T (appartient N (cdr L)))
  )
)

;;2
(defun ensemble (L)
  (cond
    ((null L) T)
    ((member(car L)(cdr L)) NIL)
    (t (ensemble (cdr L)))
  )
)

;;3
(defun inter(E1 E2)
  (if(null E1)
    NIL
    (if(member (car E1) E2)
      (append (list (car E1)) (inter (cdr E1) E2))
      (inter (cdr E1) E2)
    )
  )
)

;exo5
;;1
(defun inserElement (N L)
  (if (null L)
    (list N)
    (if(< N (car L))
      (cons N L)
      (cons (car L) (inserElement N (cdr L)))
    )
  )
)

;;2
(defun tri_insertion (L)
  (if(null L)
    NIL
    (inserElement (car L) (tri_insertion (cdr L)))
  )
)

(defun au_moins(E N L)
  (>= (count E L) N)
)

(defun plus_trois(L)
  (cond
    ((null L) L)
    ((oddp (car L)) (cons (+ 3 (car L)) (plus_trois(cdr L))))
    (t (cons (car L)(plus_trois(cdr L))))
  )
)


(defun converture(L)
  (if(null L)
    NIL
    (if(member (car L) (cdr L))
      (converture(cdr L))
      (cons (car L) (converture (cdr L)))
    )
  )
)

;Examen-sujet1
(defun lister(N)
  (if (= N 0)
    NIL
    (append (lister (- N 1)) (list N) )
  )
)

(defun transformer(L)
  (cond
    ((null L) NIL)
    ((member (car L) (cdr L)) (transformer (cdr L)))
    (t (cons (car L)(transformer(cdr L))))
  )
)


(defun dernier(L)
  (if(null L)
    NIL
    (if(null (cdr L)) (car L) (dernier (cdr L)))
  )
)

(defun plus_trois2(L)
  (mapcar (lambda(x)(if(oddp x) (+ 3 x) x)) L)
)

(defun dans(X L)
  (if(null L)
    NIL
    (if(equal X (car L)) T (dans X (cdr L)))
  )
)


(defun occur (e n L)
  (if(zerop n)
    T
    (if(null L)
      NIL
      (if(equal e (car L)) 
        (occur e (- n 1) (cdr L))
        (occur e n (cdr L))
      )
    )
  )
)


(defun extrait (L n)
  (if(null L)
    NIL
    (if(equal T (occur (car L) n L))
      (append (list (car L)) (extrait (cdr L) n))
      (extrait (cdr L) n)
    )
  )
)

(defun mystere1 (N L)
  (mapcar #'(lambda(x)(cons N (cdr x)))L)
)

(defun traiter (L)
  (mapcar(lambda(x)(if(oddp x) (* x 2) (/ x 2))) L)
)

;;transpose d'une matrice
(defun transpose (matrix)
  "Fonction récursive qui calcule la transposée d'une matrice."
  (if (null matrix)
    nil
    (append (mapcar #'car matrix)
            (transpose (mapcar #'cdr matrix))
    )
  )
)

(defun print-matrix (matrix)
  "Fonction pour afficher une matrice."
  (dolist (row matrix)
    (princ "(")
    (dolist (item row)
      (princ item))
    (princ ")")))

(defun main ()
  "Fonction principale."
  (let ((matrix (list (list 1 2 3)
                      (list 4 5 6)
                      (list 7 8 9))))
    (print-matrix matrix)
    (princ "La transposée est : ")
    (print-matrix (transpose matrix))))

(defun plus_petits (N L)
  (if (null L)
    NIL
    (if (< (car L) N) 
      (cons (car L) (plus_petits N (cdr L)))
      (plus_petits N (cdr L))
    )
  )
)