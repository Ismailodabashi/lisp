# LISP

Лисп (LISP, от англ. LISt Processing — «обработка списков») — семейство языков программирования, основанных на представлении программы системой линейных списков символов, которые притом являются основной структурой данных языка. Лисп считается вторым после Fortran старейшим высокоуровневым языком программирования.


- [Задача 8](#Задача-8)
- [Задача 13](#Задача-13)
- [Задача 15](#Задача-15)
- [Задача 25](#Задача-25)
- [Задача 28](#Задача-28)

# Задача 8

Определите функцию, которая разделит исходный список из целых чисел на два списка: список положительных чисел и список отрицательных чисел.

``` LISP
(defun task (lst &optional (p nil) (n nil))
  (cond ((null (car lst)) (list p n))
        ((> (car lst) 0) (task (cdr lst) (cons (car lst) p) n))
        ((< (car lst) 0) (task (cdr lst) p (cons (car lst) n)))
        ((= (car lst) 0) (task (cdr lst) p n))
  )
) 
```

<p>
    <img src="https://github.com/Ismailodabashi/lisp/blob/master/Задача%208.png"  width="300">
    </p>
    
# Задача 13

Определите функцию, удаляющие в исходном списке все повторные вхождения элементов.

``` LISP
(defun double (lst &optional (d nil))
    (cond ((null (car lst)) (list d))
          ((eq (member (car lst) (cdr lst)) nil) (double (cdr lst) (cons (car lst) d)))
          (t (double (cdr lst) d))
    )
)
```

<p>
    <img src="https://github.com/Ismailodabashi/lisp/blob/master/Задача%2013.png"  width="450">
    </p>
    
# Задача 15

Определите функцию, вычисляющую скалярное произведение векторов, заданных списками целых чисел.

``` LISP
(defun scpr (x y)
  (if (or (null x)(null y)) 0 (+ (* (car x) (car y)) (scpr (cdr x) (cdr y))))
)
```

<p>
    <img src="https://github.com/Ismailodabashi/lisp/blob/master/Задача%2015.png"  width="220">
    </p>
    
# Задача 25

Определите функцию, удаляющую из списка каждый четный элемент.

``` LISP
(defun chet (lst &optional (n nil))
    (cond ((null (car lst)) (list n))
          ((/= (rem (car lst) 2) 0) (chet (cdr lst) (cons (car lst) n)))
          (t (chet (cdr lst) n))
    )
 )
```

<p>
    <img src="https://github.com/Ismailodabashi/lisp/blob/master/Задача%2025.png"  width="450">
    </p>  
    
# Задача 28

Определите функцию, вычисляющую, сколько всего атомов в списке.

``` LISP
(defun atm (lst &optional (kol 0))
  (cond ((null lst) kol)
        ((atom (car lst)) (atm (cdr lst) (+ kol 1)))
        (t (atm (cdr lst) kol))
  )
)
```

<p>
    <img src="https://github.com/Ismailodabashi/lisp/blob/master/Задача%2028.png"  width="250">
    </p>   
    

    
