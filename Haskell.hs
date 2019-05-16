--1. Реализовать на языке Haskell функцию вычисления суммы элементов списка.

sumElem [] = 0
sumElem (first:mytail) = first + sumElem mytail

main = print $ sumElem [1, 2, 3]
--6

--2. Реализовать на языке Haskell функцию нахохдения максимального элемента списка.

maxElem (first:mytail) = if null mytail then first else max first $ maxElem mytail

main = print $ maxElem [1, 2, 3, 4]
-- 4
main = print $ maxElem [-1, -2, -3, -5]
-- -1

--3. Реализовать на языке Haskell функцию, удаляющую из исходного списка элементы с четными номерами

delEven [] = []
delEven (first:mytail) = first : (delEven $ tail mytail) 

main = print $ delEven [1, 2, 3, 4, 5, 6]
-- [1,3,5]

--4. Реализовать на языке Haskell функцию, которая разделяет исходный список из целых чисел на два
--списка: список положителтных чисел и список отрицательных чисел.

getposneg [] = []
getposneg lst = ([filter (<0) lst]) ++ ([filter (>0) lst])
                     
main = print $ getposneg [-1, 2, -2, 4, 5, 0, 1]
-- [[-1,-2],[2,4,5,1]]

--5. Реализовать на языке Haskell функцию, заменяющую в исходном списке два подряд идущих одинаковых
--элемента одним.

repl2Identical [] = []
repl2Identical (first:[]) = [first]
repl2Identical (first:mytail) = if (first == head mytail)
                     then first : repl2Identical (tail mytail)
                     else first : repl2Identical  mytail
                     
main = print $ repl2Identical [1,1,2,2,3,3,4,5]
-- [1,2,3,4,5]

--6. Реалиpовать на языке Haskell функцию, которая преобразует исходный список в список, в котором элементы написаны в обратном к исходному порядке. 

backlist [] = []
backlist (first:[]) = [first]
backlist (first:mytail) = backlist mytail ++ [first]
                     
main = print $ backlist [1, 2, 3, 4, 5, 6]
-- [6,5,4,3,2,1]

--7. Реализовать на языке Haskell функцию, заменяющую в исходном списке все вхождения заданного значения другим.

replace a b [] = []
replace a b (first:mytail) = if (a == first)
                             then b : (replace a b mytail)
                             else first : (replace a b mytail)
                     
main = print $ replace 1 2 [1, 2, 3, 4, 5, 6, 1]

-- [2,2,3,4,5,6,2]

--8. Реализовать на языке Haskell функцию, которая увеличивает элементы исходного списка на единицу

allplus1 [] = []
allplus1 lst = map (1+) lst
                     
main = print $ allplus1 [1, 2, 3, 4, 5, 6, 1]

-- [2,3,4,5,6,7,2]

--9. Реализовать на языке Haskell функцию, которая увеличивает элементы исходного списка в 10 раз.

allplus1 [] = []
allplus1 lst = map (10*) lst
                     
main = print $ allplus1 [1, 2, 3, 4, 5, 6, 1]

-- [10,20,30,40,50,60,10]

--10. Реализовать на языке Haskell функцию, вычисляющую скалярное произведение векторов, заданных списками целых чисел.

getScalar [][] = 0
getScalar x y =  foldr (+) 0 (zipWith (*) x y)
                     
main = print $ getScalar [1, 2, 3, 4, 5] [1, 3, 4, 5, 2]

-- 49

