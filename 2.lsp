;1. Определите FUNCALL через функционал APPLY.




;3.Определите функционал (APL-APPLY f x), который применяет каждую функцию fi списка (f1 f2 ... fn)
;к соответствующему элементу списка x = (x1 x2 ... xn) и возвращает список, сформированный из результатов.

;переделать через лямбды

(defun APL-APPLY(f x)
    ( cond
        ((null f) nil )
        ((atom f) (cons (funcall (car f) (car x)) (apl-apply (cdr f) (cdr x))) )
         (t (cons (apply (car f) (car x)) (apl-apply (cdr f) (cdr x))) )
    )   
)

(print (apl-apply '(+ max * ) '( (1 3) (1 2 3 4) '(1 2) ) ) )
