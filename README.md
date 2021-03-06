# SPL

# LISP #
## Первый блок. Основы языка LISP ##
**(6 16 22 23 26 30 32 34 33 47)**
+ Определите функцию, переводящую список чисел в список соответствующих им названий.
+ Определите функцию, добавляющую элементы одного списка во второй список, начиная с заданной позиции.
+ Определите функцию, которая обращает список (а b с) и разбивает его на уровни (((с) b) а).
+ Определите функции, преобразующие список (a b с) к виду (а (b (с))) и наоборот.
+ Определите функцию, разбивающую список (a b с d...) на пары ((а b) (с d)...).
+ Запрограммируйте интерпретатор ВЫЧИСЛИ, который преобразует инфиксную запись операций в префиксную и возвращает значение выражений.
+ Определите предикат МНОЖЕСТВО-Р, который проверяет, является ли список множеством, т.е. входит ли каждый элемент в список лишь один раз.
+ Определите функцию МНОЖЕСТВО, преобразующую список в множество.
+ Определите предикат РАВЕНСТВО-МНОЖЕСТВ, проверяющий совпадение двух множеств (независимо от порядка следования элементов). Подсказка: напишите функцию УДАЛИТЬ, удаляющую данный элемент из множества.
+ Определите функцию УДАЛИТЬ-ВСЕ-СВОЙСТВА, которая удаляет все свойства символа.

## Второй блок. Функции высших порядков ##
**(1 3 5 7 9 11 13)**
+ Определите FUNCALL через функционал APPLY.
+ Определите функционал (APL-APPLY f x), который применяет каждую функцию fi списка (f1 f2 ... fn) к соответствующему элементу списка x = (x1 x2 ... xn) и возвращает список, сформированный из результатов.
+ Определите функциональный предикат (НЕКОТОРЫй пред список), который истинен, когда, являющейся функциональным аргументом предикат *пред* истинен хотя бы для одного элемента списка *список*.
+ Определите фильтр (УДАЛИТЬ-ЕСЛИ-НЕ пред список), удаляющий из списка *список* все элементы, которые не обладают свойством, наличие которого проверяет предикат *пред*.
+ Напишите генератор порождения чисел Фибоначчи: 0, 1, 1, 2, 3, 5, ...
+ Определите фукнционал МНОГОФУН, который использует функции, являющиеся аргументами, по следующей схеме:
(МНОГОФУН ’(f g ... h) x) ⇔ (LIST (f x) (g x) ... (h x)).
+ Определите функцию, которая возвращает в качестве значения свое определение (лямбда-выражение).

## Третий блок. Макросы ##
+ Определите макрос, который возвращает свой вызов.
+ Определите макрос (POP стек), который читает из стека верхний элементи меняет значение переменной стека.
+ Определите лисповскую форму (IF условие p q) в виде макроса.
+ Определте в виде макроса форму (FIF тест отр нуль полож).
+ Определте в виде макроса форму (REPEAT e UNTIL p) паскалевского типа.
+ Разработать "собственный" ЯП.

# HASKELL #
**Cписки: (10, 12, 13, 18, 23), коды: 2, деревья: 4**


