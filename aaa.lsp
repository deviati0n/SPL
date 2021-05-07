; минимальное сред максимальных значений в строках матрицы

(defun maxv (lst) 
    ((lambda (x) x )(apply #'max lst))
)
 
(defun find-min-max (lst)
    (apply 'min (mapcar #'maxv lst))
)

 
(print (find-min-max '((1 2 30) (4 5 15) (7 8 9))))
