;Задача 8
;Определите функцию, которая разделит исходный список из целых 
;чисел на два списка: список положительных чисел и список отрицательных чисел.

(defun task (lst)
    (if (null lst) nil
           ((lambda (first last tasklast)
                         ((lambda (poslst neglst)
                                        (cond
                                            ((< first 0) (list poslst (cons first neglst)))
                                            ((> first 0) (list (cons first poslst) neglst))
                                            ((= first 0) tasklast)       
                                        )
                           )
                           (car tasklast)
                           (cadr tasklast)
                          )
             )
            (car lst)
            (cdr lst)
            (task (cdr lst))
            )
     )
)




;(task '(1 2 3 4 0 -2 3 -3))
;((1 2 3 4 3)(-2 -3))
;(task '(-2 3 -4 3 2 -5 0 0 3))
;((3 2 3 3)(-5 -4 -2))



;Задача 13
;Определите функцию, удаляющие в исходном списке все повторные вхождения элементов.

(defun deldubl (lst)
    (if (null lst) nil
        ((lambda (first last funlast)   
                 (cond ((eq (member first funlast) nil) (cons first funlast))
                       (t funlast)
                 )  
          )
          (car lst)(cdr lst)(deldubl (cdr lst))
        )
     )
)

;(deldubl '(9 1 3 2 1 2 2 2 3 2 3 4 1 5 4 6 3))
;(3 6 4 5 1 2 9)
;(deldubl '(-1 2 2 1 0 0 3 4 -1))
;(-1 4 3 0 1 2)


;Задача 15
;Определите функцию, вычисляющую скалярное произведение векторов, заданных списками целых чисел.


(defun scalar (x y)
  (if (or (null x)(null y)) 0 (+ (* (car x) (car y)) (scalar (cdr x) (cdr y))))
)


;(scalar '(1 2) '(2 3))
;8
;(scalar '(0 2) '(9 2))
;4



;Задача 21
;Определите функцию, удаляющую из списка первое вхождение данного элемента на верхнем уровне.


(defun del (lst a)
    ((lambda (first last)
             (if (atom first)
              	(cond  ((null first) lst)
                       ((= first a) last)
                       (t (cons first (del last a)))
                )
                 (cons first (del last a))
              )				  
     ) 
     (car lst) (cdr lst)
    )
)

;(del '(1 2 3 4 5 3) 3)
;(1 2 4 5 3)
;(del '(4 2 1 2 3 2) 5)
;(4 2 1 2 3 2)


;Задача 25
;Определите функцию, удаляющую из списка каждый четный элемент.

(defun even (lst)
    (if (null lst) nil
        ((lambda (first last evlast) 
              (cond ((/= (rem first 2) 0) (cons first evlast))
                    (t evlast)
              )
         )
         (car lst)
         (cdr lst)
         (even (cdr lst))
        )
    )
 )
 
 ;(even '(9 1 3 2 1 2 2 2 3 2 3 4 1 5 4 6 3))
 ;(3 5 1 3 3 1 3 1 9)
 ;(even '(1 2 3 4 5 6 7 8 9))
 ;(9 7 5 3 1)
 
 ;Задача 28
 ;Определите функцию, вычисляющую, сколько всего атомов в списке.
 
(defun choiceatoms (lst)
    (if (null lst) nil
        ((lambda (first last numblast) 
                 (cond ((null lst) nil)
                       ((atom first) (cons 1 numblast))
                       (t (cons 0 numblast))
                 )
         )
         (car lst) (cdr lst)(choiceatoms (cdr lst))
         ) 
    )
)


(defun sumlist (lst)
    (if (null lst) 0
      (+ (car lst) (sumlist (cdr lst)))
    )
)

(defun numbatoms (lst)
    (sumlist (choiceatoms lst))
)

;(numbatoms '(1 2 3 4 (5 6)))
;4
;(numbatoms '((1 2 3)(2 3)))
;0

;Задача 32
;Определить предикат МНОЖЕСТВО, который проверяет, является ли список множеством


(defun ismany (lst)
	((lambda (first last) 
              (cond ((null lst) t)
			        ((member first last) nil)
			        (t (ismany last))
		      )
     ) (car lst)(cdr lst))
		
)

;(ismany '(1 2 2 3 4 5))
;FALSE
;(ismany '(4 3 5 2 7 0 1))
;TRUE

;Задача 45
;Напишите функцию (РАССТОЯНИЕ a b), вычисляющую расстояние между городами a b.


(defun prop (city xy)
	(setf (get city 'x) (car xy))
	(setf (get city 'y) (cadr xy))
)


(defun dist (city1 city2)
    ((lambda (x1 y1 x2 y2) 
             (sqrt(+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1))))
     ) 
     (get city1 'x)
     (get city1 'y)
     (get city2 'x)
     (get city2 'y)
    )
)

;(prop 'Simf '(-10 2))
;(prop 'Bahch '(20 -25))
;(dist 'Simf 'Bahch)
;40.36087

;Задача 46
;Напишите функцию (РОДИТЕЛИ x) и (СЕСТРЫ_БРАТЬЯ x1 x2).

(defun prop (name dadmom)
    (setf (get name 'dad) (car dadmom))
    (setf (get name 'mom) (cadr dadmom))
)

(defun get-parents (x)
    (cons (get x 'dad) (get x 'mom))
)

(defun are-they-siblings (x1 x2)
    (if (or (eq (get x1 'dad) (get x2 'dad)) (eq (get x1 'mom) (get x2 'mom))) t nil)
)

;(prop 'Ismail '(Ernes Elzara))
;(prop 'Enver '(Ernes Elzara))
;(print (get-parents 'Ismail))
;(print (get-parents 'Enver))
;(print (are-they-siblings 'Ismail 'Enver))
;T 



