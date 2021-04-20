-- 13. Определите функцию, которая, чередуя элементы списков (a b...) и (1 2...), образует новый список (a 1 b 2 ...).

cher x y 
        | x == [] = y
        | y == [] = x
        | otherwise = [(head x)] ++ [(head y)] ++ cher (tail x) (tail y)

main :: IO ()
main = do
    print "13. (a b...) & (1 2..) -> (a 1 b 2 ...)"
    print (cher [1, 3, 5] [2, 4, 6])
    print (cher [1, 3, 5] [2, 4, 6, 7])
    print (cher [1, 3, 5, 7] [2, 4, 6])

-- 18. Определите предикат РАВЕНСТВО-МНОЖЕСТВ, проверяющий совпадение двух множеств (независимо от порядка следования элементов).
-- Подсказка: напишите функцию УДАЛИТЬ, удаляющую данный элемент из множества.    

delete x member
        | x == [] = []
        | member == (head x) = delete (tail x) member
        | otherwise = (head x) : delete (tail x) member
eq_set x y
        | x == [] && y == [] = True
        | (length x == length y) == False = False
        | otherwise = eq_set (tail x) (delete y (head x))
        
            
main :: IO ()
main = do
    print "18. equality check of two sets"
    print (eq_set [1, 3, 5] [2, 4, 6])
    print (eq_set [1, 3, 5] [5, 3, 1])
    print (eq_set [1, 3, 5] [3, 1, 5, 6])
