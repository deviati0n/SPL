;1.Определите FUNCALL через функционал APPLY.
(defun funcall-apply(fn &rest args)
    ( cond
          ((null args) nil)
          (t (apply fn args) )
    ) 
)

(print "1.Определите FUNCALL через функционал APPLY.")
(print (funcall-apply '+ 2 3))
(print (funcall-apply 'max 2 3 14 9 ))
(print (funcall-apply 'cdr '(2 3 4 5) ))


;3. Определите функционал (APL-APPLY f x), который применяет каждую функцию fi списка (f1 f2 ... fn) к соответствующему
;элементу списка x = (x1 x2 ... xn) и возвращает список, сформированный из результатов.

(defun APL-APPLY(f x)
    ( ( lambda (first-func rest-func first-x rest-x) 
        ( cond
            ((null f) nil )
            ((atom first-x) (cons (funcall first-func first-x) (apl-apply rest-func rest-x)) )
            (t (cons (apply first-func first-x ) (apl-apply rest-func rest-x )) )
        ) 
      ) (car f) (cdr f) (car x) (cdr x) 
    )  
)

(print "3. Определите функционал (APL-APPLY f x), который применяет каждую функцию fi списка (f1 f2 ... fn) к соответствующему элементу списка x = (x1 x2 ... xn) и возвращает список, сформированный из результатов.")
(print (apl-apply '(+ max * ) '( (1 3) (1 2 3 4) (1 2) ) ) )
(print (apl-apply '(- atom / ) '( (1 3) A (4 2) ) ) )
(print (apl-apply '(null) '( A ) ) )

;5. Определите функциональный предикат (НЕКОТОРЫй пред список), который истинен,
;когда, являющейся функциональным аргументом предикат пред истинен хотя бы для одного элемента списка список.

(defun НЕКОТОРЫЙ (пред список) 
    (not (null (mapcan #'(lambda (x) (if (funcall пред x) (list t) nil) ) список)))
)

(print "5. Определите функциональный предикат (НЕКОТОРЫй пред список), который истинен, когда, являющейся функциональным аргументом предикат пред истинен хотя бы для одного элемента списка список.")
(print (НЕКОТОРЫЙ (lambda (x) (> x 0)) '(-1 2 -3)) )
(print (НЕКОТОРЫЙ (lambda (x) (atom x)) '((1 2) (3 4) (a b))) )
(print (НЕКОТОРЫЙ (lambda (x) (numberp x)) '(1 a b c)) )

;7. Определите фильтр (УДАЛИТЬ-ЕСЛИ-НЕ пред список), удаляющий из списка список все элементы,
;которые не обладают свойством, наличие которого проверяет предикат пред.

(defun УДАЛИТЬ-ЕСЛИ-НЕ (пред список) 
    (mapcan #'(lambda (x) (if (funcall пред x) (list x) nil) ) список)
)

(print "7. Определите фильтр (УДАЛИТЬ-ЕСЛИ-НЕ пред список), удаляющий из списка список все элементы, которые не обладают свойством, наличие которого проверяет предикат пред.")
(print (УДАЛИТЬ-ЕСЛИ-НЕ (lambda (x) (> x 0)) '(1 2 -3)) )
(print (УДАЛИТЬ-ЕСЛИ-НЕ (lambda (x) (atom x)) '((1 2) a (a b))) )
(print (УДАЛИТЬ-ЕСЛИ-НЕ (lambda (x) (numberp x)) '(1 a b c)) )

;9. Напишите генератор порождения чисел Фибоначчи: 0, 1, 1, 2, 3, 5, ...
(defun fibonacci ()
    (let ((a 1)(b 1)(c -1))
         (lambda ()
                 (setq c (cond 
                             ((< c 0) 0)
                             ((+ (setq a b) (setq b c)))
                          )
                 )
         )
    )        
)


(print "9. Напишите генератор порождения чисел Фибоначчи: 0, 1, 1, 2, 3, 5, ...")
(setq c1 (fibonacci))
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))
(print (funcall c1))


;11. Определите фукнционал МНОГОФУН, который использует функции,
;являющиеся аргументами, по следующей схеме: (МНОГОФУН ’(f g ... h) x) ⇔ (LIST (f x) (g x) ... (h x)).

(defun МНОГОФУН (func x)
    ( cond
          ((null func) nil)
          (t (cons (funcall (car func) x) (МНОГОФУН (cdr func) x) ) )
    ) 
)

(print "11. Определите фукнционал МНОГОФУН, который использует функции, являющиеся аргументами, по следующей схеме: (МНОГОФУН ’(f g ... h) x) ⇔ (LIST (f x) (g x) ... (h x)).")

(print ( МНОГОФУН '(tan  abs) -12 ))
(print ( МНОГОФУН '(cos sin atom) 45 ))
(print ( МНОГОФУН '(car cdr cadr) '(1 2 3 4)))

(defun МНОГОФУН (func x) 
    (mapcar #'(lambda (f) (funcall f x )) func)
)

(print "Второй вариант решения данной задачи")
(print ( МНОГОФУН '(tan  abs) -12 ))
(print ( МНОГОФУН '(cos sin atom) 45 ))
(print ( МНОГОФУН '(car cdr cadr) '(1 2 3 4)))

;13. Определите функцию, которая возвращает в качестве значения свое определение (лямбда-выражение).

(defun defin-val ()
    ((lambda (val) 
             (list val (list 'quote val))
      )'(lambda (val)
                     (list val (list 'quote val))
             )
     )
)

(print "13. Определите функцию, которая возвращает в качестве значения свое определение (лямбда-выражение).")
(print (defin-val))

