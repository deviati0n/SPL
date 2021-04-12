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
    '(prog1  (car stack)
            (setq stack (cdr stack))
    )
)

(print "2. Определите макрос (POP стек), который читает из стека верхний элементы и меняет значение переменной стека.")

(setq stack '(a b))
(print (POP-STACK stack))
(print stack)

(setq stack '(1 2 3))
(print (POP-STACK stack))
(print stack)

(setq stack '(+))
(print (POP-STACK stack))
(print stack)

;3. Определите лисповскую форму (IF условие p q) в виде макроса.

(defmacro if-m (условие p q)
    `(if ,условие ,p ,q)
)

(print "3. Определите лисповскую форму (IF условие p q) в виде макроса.")

(setq x '5)
(print (if-m (numberp x) (* x x) 'not_a_number))

(setq x '(1 2 3))
(print (if-m (listp x) (mapcar 'list x) (cons x '(just an atom))))

(setq x 'a)
(print (if-m (listp x) (mapcar 'list x) (cons x '(- just an atom))))

;4. Определте в виде макроса форму (FIF тест отр нуль полож).


;5. Определте в виде макроса форму (REPEAT e UNTIL p) паскалевского типа.

;6. Разработать "собственный" ЯП.
