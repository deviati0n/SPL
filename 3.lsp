;1. Определите макрос, который возвращает свой вызов.

(defmacro own-call (&whole whole &rest args)
    `(quote ,whole)
)

(print "1. Определите макрос, который возвращает свой вызов.")

(print (own-call '(a b c d) 5 func))
(print (own-call nil))
(print (own-call))


;2. Определите макрос (POP стек), который читает из стека верхний элемент и меняет значение переменной стека.

(defmacro POP-STACK (стек)
      `(prog1                          ;возвращает значение первого аргумента
               (car стек)
               (setq стек (cdr стек))
       )
)

(print "2. Определите макрос (POP стек), который читает из стека верхний элементы и меняет значение переменной стека.")

(setq стек '(a b))
(print (POP-STACK стек))
(print стек)

(setq стек '(1 2 3))
(print (POP-STACK стек))
(print стек)

(setq стек '(+))
(print (POP-STACK стек))
(print стек)

;3. Определите лисповскую форму (IF условие p q) в виде макроса.

(defmacro if-m (условие p q)
    `(if ,условие ,p ,q)     ;`(cond (,условие ,p) (t ,q))
)


(print "3. Определите лисповскую форму (IF условие p q) в виде макроса.")

(setq x '5 y '(1 2 3) z 'a)

(print (if-m (numberp x) (* x x) 'not_a_number))
(print (if-m (listp y) (mapcar 'list y) (cons y '(- just an atom))))
(print (if-m (listp z) (mapcar 'list z) (cons z '(- just an atom))))

;4. Определте в виде макроса форму (FIF тест отр нуль полож).

(defmacro FIFS (тест отр нуль полож)
    `(cond
         ((< ,тест 0) ,отр)
         ((> ,тест 0) ,полож)
         (t ,нуль)         
     )
)


(print "4. Определте в виде макроса форму (FIF тест отр нуль полож).")

(print (FIFS -4 `отр `нуль `полож))
(print (FIFS 0 `отр `нуль `полож))
(print (FIFS 4 `отр `нуль `полож))


;5. Определте в виде макроса форму (REPEAT e UNTIL p) паскалевского типа.

;6. Разработать "собственный" ЯП.
