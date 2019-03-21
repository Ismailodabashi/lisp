# LISP

Лисп (LISP, от англ. LISt Processing — «обработка списков») — семейство языков программирования, основанных на представлении программы системой линейных списков символов, которые притом являются основной структурой данных языка. Лисп считается вторым после Fortran старейшим высокоуровневым языком программирования.


- [Задача 8](#Задача-8)
- [Задача 13](#Задача-13)
- [Задача 15](#Задача-15)
- [Задача 21](#Задача-21)
- [Задача 25](#Задача-25)
- [Задача 28](#Задача-28)

# Задача 8

Определите функцию, которая разделит исходный список из целых чисел на два списка: список положительных чисел и список отрицательных чисел.

``` LISP
(defun task (lst &optional (p nil) (n nil))
  ((lambda (x) (and(setq first (car x))(setq last (cdr x)))) lst)
      (cond ((null first) (list p n))
            ((> first 0) (task last (cons first p) n))
            ((< first 0) (task last p (cons first n)))
            ((= first 0) (task last p n))
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
      ((lambda (x) (and(setq first (car x))(setq last (cdr x)))) lst)
            (cond ((null first) (list d))
                  ((eq (member first last) nil) (double last (cons first d)))
                  (t (double last d))
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
    
# Задача 21

Определите функцию, удаляющую из списка первое вхождение данного элемента на верхнем уровне.

``` LISP
(defun udal (lst a)
    ((lambda (x) (and(setq first (car x))(setq last (cdr x)))) lst)
        (cond ((null first) lst)
              ((= first a) last)
              (t (cons first (udal (cdr lst) a))))
        )

(print(udal '(2 1 3 2 3 4) 2))
```

<p>
    <img src="https://github.com/Ismailodabashi/lisp/blob/master/Задача%2021.png"  width="250">
    </p>  

# Задача 25

Определите функцию, удаляющую из списка каждый четный элемент.

``` LISP
(defun chet (lst &optional (n nil))
    ((lambda (x) (and(setq first (car x))(setq last (cdr x)))) lst)
        (cond ((null first) (list n))
              ((/= (rem first 2) 0) (chet last (cons first n)))
              (t (chet last n))
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
    ((lambda (x) (and(setq first (car x))(setq last (cdr x)))) lst)
      (cond ((null lst) kol)
            ((atom first) (atm last (+ kol 1)))
            (t (atm last kol))
      )
)
```

<p>
    <img src="https://github.com/Ismailodabashi/lisp/blob/master/Задача%2028.png"  width="250">
    </p>   
    

    
