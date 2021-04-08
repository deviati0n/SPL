;6. Определите функцию, переводящую список чисел в список соответствующих им названий.

(set `num `(   (0 ноль) (1 один) (2 два) (3 три) (4 четыре) (5 пять) (6 шесть) (7 семь) (8 восемь) (9 девять)
             (10 десять) (11 одиннадцать) (12 двенадцать) (13 тринадцать) (14 четырнадцать) (15 пятнадцать) (16 шестнадцать) (17 семнадцать) (18 восемнадцать) (19 девятнадцать)
             (20 двадцать) (30 тридцать) (40 сорок) (50 пятьдесят) (60 шестьдесят) (70 семьдесят) (80 восемьдесят) (90 девяносто) (100 сто) (200 двести) (300 триста) (400 четыреста) 
             (500 пятьсот) (600 шестьсот) (700 семьсот) (800 восемьсот) (900 девятьсот)
         )
)


(defun pars-num ( x &optional (des 10))
    ((lambda (remX) 
        ( cond 
            ((= x 0) nil)
            ((= remX 0) (pars-num (- x remX) ( * des 10) ))
            (t (cons remX (pars-num(- x remX) ( * des 10) )))
        )
     ) (rem x des))
)


(defun search-num (x num)
	(cond 
        ((null num) nil)
		((equal x (caar num)) (cadar num))
		(t (search-num x (cdr num)))	
	)
)

(defun num-string (x)
        
    (cond ((null x) nil)
        (t (cons (search-num (car x) num) (num-string (cdr x))))		 
    )
    
)


(defun num-to-string (x) 
    ((lambda (first rest search-n)
        (cond 
            ((null x) nil)
            (search-n (cons search-n (num-to-string rest)) )
            (t (cons (num-string (reverse (pars-num first))) (num-to-string rest)))		 
        )
    )(car x)(cdr x)(search-num (car x) num))
)

(print "6. Определите функцию, переводящую список чисел в список соответствующих им названий.")
(print (num-to-string '(123 320 944 5 100 82 1)))
(print (num-to-string '(12 10 3 193)))
(print (num-to-string '(900 2 739)))




;16. Определите функцию, добавляющую элементы одного списка во второй список, начиная с заданной позиции.

(defun ins-list(L1 L2 num)
     (cond 
         ((null L2) L1)
         ((= num 0) ( cons (car L2) (ins-list L1 (cdr L2) num)))
         (t (cons (car L1) (ins-list (cdr L1) L2 (- num 1))))         
     )
)


(print "16. Определите функцию, добавляющую элементы одного списка во второй список, начиная с заданной позиции.")
( print (ins-list '(a b f g h) '(c d e) 2 ) )
( print (ins-list '(f g h) '(a b c d e) 0 ) )
( print (ins-list '( ) '(a b c d e) 0 ) )

;22. Определите функцию, которая обращает список (а b с) и разбивает его на уровни (((с) b) а).

(defun create-list-in-list (L)
    ( ( lambda (p rest)
               ( cond
                   ( (null rest) (list p) )
                   ( t (list ( create-list-in-list rest ) p  ) )
               )   
        ) (car L) (cdr L)
     )
)


(print "22. Определите функцию, которая обращает список (а b с) и разбивает его на уровни (((с) b) а).")
( print ( create-list-in-list '(12 10 13 14 32) ) )
( print ( create-list-in-list '(a) ) )
( print ( create-list-in-list '(a b c) ) )

;23. Определите функции, преобразующие список (a b с) к виду (а (b (с))) и наоборот.
 
(defun create-list-in-list (L)
    ( ( lambda (p rest)
               ( cond
                   ( (null rest) (list p) )
                   ( t (list p ( create-list-in-list rest ) ) )
               )   
        ) (car L) (cdr L)
     )
)

(defun create-list-of-list (L)
    ( ( lambda (p rest)
               ( cond
                   ( (null rest) (list p)  )
                   ( t (cons p ( create-list-of-list rest )  ) )
               )   
        ) (car L) (cadr L)
     )
)


(print "23. Определите функции, преобразующие список (a b с) к виду (а (b (с))) и наоборот.")
( print ( create-list-of-list '(a (b (c))) ) )
( print ( create-list-of-list '(A (B (C (D (E (F)))))) ) )
( print ( create-list-of-list '(a) ) )

( print ( create-list-in-list '(a b c) ) )
( print ( create-list-in-list '(a) ) )
( print ( create-list-in-list '(a b c d e f) ) )

;26. Определите функцию, разбивающую список (a b с d...) на пары ((а b) (с d)...).

(defun create-pair(L)
    ( ( lambda (p1 p2 rest )
               ( cond
                   ;((= (list-length L) 1)  L ) 
                   ((= (list-length L) 1)  (list L) ) ;две строчки для обработки последнего элемента, если их нечетное кол-во
                   ((null rest)  (cons (list p1 p2 ) rest) ) 
                   (t (cons (list p1 p2 ) (create-pair rest)) )
               )   
        ) (car L) (cadr L) (cddr L)
     )
)


(print "26. Определите функцию, разбивающую список (a b с d...) на пары ((а b) (с d)...).")
( print ( create-pair '(12 10 13 145) ) )
( print ( create-pair '(12 10) ) )
( print ( create-pair '(12 10 21) ) )	

;30. Запрограммируйте интерпретатор ВЫЧИСЛИ, который преобразует инфиксную запись операций в префиксную и возвращает значение выражений.

(defun ВЫЧИСЛИ(L)
    ( cond
        ((null L) nil )
        ((atom L) L)
        (t (list (cadr L) (ВЫЧИСЛИ (car L)) (ВЫЧИСЛИ (caddr L)) ))
    )   
)

(print "30. Запрограммируйте интерпретатор ВЫЧИСЛИ, который преобразует инфиксную запись операций в префиксную и возвращает значение выражений.")
( print ( ВЫЧИСЛИ '((2 * 3) + (3 * 6))) )
( print (eval ( Вычисли '((2 * 3) + (3 * 6)) )))

( print ( ВЫЧИСЛИ '((2 * (23 + 5)) + (3 * 6)) ))
( print (eval ( Вычисли '((2 * (23 + 5)) + (3 * 6)) )))

( print ( ВЫЧИСЛИ '(1 + (2 + (3 + (4 + 5)))) ))
( print (eval ( Вычисли '(1 + (2 + (3 + (4 + 5)))) )))

;32. Определите предикат МНОЖЕСТВО-Р, который проверяет, является ли список множеством, т.е. входит ли каждый элемент в список лишь один раз.

;функция member 
(defun is-member (x list)
     ( (lambda (p rest)
              (cond
                  ((null list) nil)
                  ((equal p x) t)
                  (t (is-member x rest ))
              )
       )(car list) (cdr list)
    )
)

(defun МНОЖЕСТВО-Р (L) 
    (   (lambda (p rest)
            (cond 
                  ((null L) "множество" )
                  ( (is-member p rest ) "не множество" )
                  ( t (МНОЖЕСТВО-Р rest) )

            )
        )(car L) (cdr L)
    )
)


(print "32. Определите предикат МНОЖЕСТВО-Р, который проверяет, является ли список множеством, т.е. входит ли каждый элемент в список лишь один раз.")
(print ( МНОЖЕСТВО-Р '(1 2 3 4 1) ) )
(print ( МНОЖЕСТВО-Р '(a bc d ) ) )
(print ( МНОЖЕСТВО-Р '() ) )


;33. Определите функцию МНОЖЕСТВО, преобразующую список в множество.

(defun МНОЖЕСТВО (L) 
    (   (lambda (p rest)
            (cond 
                  ((null L) nil)
                  ( (not (member p rest )) (cons p (МНОЖЕСТВО rest)) )
                  ( t (МНОЖЕСТВО rest) )

            )
        )(car L) (cdr L)
    )
)

(print "33. Определите функцию МНОЖЕСТВО, преобразующую список в множество.")
(print ( МНОЖЕСТВО '(1 3 2 2 3 a c a) ) )
(print ( МНОЖЕСТВО '(1 a a a 1) ) )
(print ( МНОЖЕСТВО '(a b c) ) )


;34. Определите предикат РАВЕНСТВО-МНОЖЕСТВ, проверяющий совпадение двух множеств (независимо от порядка следования элементов). Подсказка: напишите функцию УДАЛИТЬ, удаляющую данный элемент из множества.

(defun УДАЛИТЬ (L member)
    ( (lambda (p rest)
         (cond
             ((null L) nil)
             ((equal member p) (УДАЛИТЬ rest member ))
             (t (cons p (УДАЛИТЬ rest member )))  
         )
        ) (car L) (cdr L)
    )
)

(defun РАВЕНСТВО-МНОЖЕСТВ (L1 L2) 
    ((lambda (first1 rest1 first2 rest2)
        (cond 
            ((and (null L1) (null L2)) "множества равны")
            (( not (= (list-length L1) (list-length L2)) ) "множества не равны")    
            ( t (РАВЕНСТВО-МНОЖЕСТВ rest1 (УДАЛИТЬ L2 first1)) )

        )
    )(car L1) (cdr L1) (car L2) (cdr L2))
)

(print "34. Определите предикат РАВЕНСТВО-МНОЖЕСТВ, проверяющий совпадение двух множеств (независимо от порядка следования элементов). Подсказка: напишите функцию УДАЛИТЬ, удаляющую данный элемент из множества.")
(print ( РАВЕНСТВО-МНОЖЕСТВ '(1 2 c 3 4 a ) '(3 2 a 4 c 1) ) )
(print ( РАВЕНСТВО-МНОЖЕСТВ '(1 2 c 3 4 a ) '(3 2 a 4 c 1 b) ) )
(print ( РАВЕНСТВО-МНОЖЕСТВ '(a b c d ) '(d c b a) ) )



;47. Определите функцию УДАЛИТЬ-ВСЕ-СВОЙСТВА, которая удаляет все свойства символа.

(defun УДАЛИТЬ-ВСЕ-СВОЙСТВА(symbol)
    ( (lambda(list)
         ( cond 
             ((null list ) t )
             (t (remprop symbol (car list)) (УДАЛИТЬ-ВСЕ-СВОЙСТВА symbol)  )
         )
    )(SYMBOL-PLIST symbol))
)

(print "47. Определите функцию УДАЛИТЬ-ВСЕ-СВОЙСТВА, которая удаляет все свойства символа.")

;Задаем св-ва элементу
(setf (get 'Mary 'age)28)
(setf (get 'Mary 'country)'Germany)
(setf (get 'Mary 'fingers)20)

(УДАЛИТЬ-ВСЕ-СВОЙСТВА 'Mary)

(print (SYMBOL-PLIST 'Mary))
