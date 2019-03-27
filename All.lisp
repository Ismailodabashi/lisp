;Задача 8
;Определите функцию, которая разделит исходный список из целых 
;чисел на два списка: список положительных чисел и список отрицательных чисел.

(defun task (lst)
    (if (null (car lst)) () (setq next (task (cdr lst))))
           ((lambda (x) (and(setq first (car x))(setq last (cdr x)))) lst)
                (cond   ((null first) (list () ()))
                        ((< first 0) (cons (car next) (list (cons first (cadr next)))))
                        ((> first 0) (cons (cons first (car next)) (cdr next)))
                        ((= first 0) next)
                )
             
           
    
        
)



(defun task (lst &optional (p nil) (n nil))
  ((lambda (x) (and(setq first (car x))(setq last (cdr x)))) lst)
      (cond ((null first) (list p n))
            ((> first 0) (task last (cons first p) n))
            ((< first 0) (task last p (cons first n)))
            ((= first 0) (task last p n))
      )
) 




;(task '(1 2 3 4 0 -2 3 -3))
;((1 2 3 4 3)(-2 -3))
;(task '(-2 3 -4 3 2 -5 0 0 3))
;((3 2 3 3)(-5 -4 -2))



;Задача 13
;Определите функцию, удаляющие в исходном списке все повторные вхождения элементов.

(defun double (lst &optional (d nil))
      ((lambda (x) (and(setq first (car x))(setq last (cdr x)))) lst)
            (cond ((null first) (list d))
                  ((eq (member first last) nil) (double last (cons first d)))
                  (t (double last d))
            )
)

;(double '(9 1 3 2 1 2 2 2 3 2 3 4 1 5 4 6 3))
;(3 6 4 5 1 2 9)
;(double '(-1 2 2 1 0 0 3 4 -1))
;(-1 4 3 0 1 2)


;Задача 15
;Определите функцию, вычисляющую скалярное произведение векторов, заданных списками целых чисел.


(defun scpr (x y)
  (if (or (null x)(null y)) 0 (+ (* (car x) (car y)) (scpr (cdr x) (cdr y))))
)


;(scpr '(1 2) '(2 3))
;8
;(scpr '(0 2) '(9 2))
;4



;Задача 21
;Определите функцию, удаляющую из списка первое вхождение данного элемента на верхнем уровне.


(defun udal (lst a)
    ((lambda (x) (and(setq first (car x))(setq last (cdr x)))) lst)
        (cond ((null first) lst)
              ((= first a) last)
              (t (cons first (udal (cdr lst) a))))

)

;(udal '(1 2 3 4 5 3) 3)
;(1 2 4 5 3)
;(udal '(4 2 1 2 3 2) 5)
;(4 2 1 2 3 2)


;Задача 25
;Определите функцию, удаляющую из списка каждый четный элемент.

(defun chet (lst &optional (n nil))
    ((lambda (x) (and(setq first (car x))(setq last (cdr x)))) lst)
        (cond ((null first) (list n))
              ((/= (rem first 2) 0) (chet last (cons first n)))
              (t (chet last n))
        )
 )
 
 ;(chet '(9 1 3 2 1 2 2 2 3 2 3 4 1 5 4 6 3))
 ;(3 5 1 3 3 1 3 1 9)
 ;(chet '(1 2 3 4 5 6 7 8 9))
 ;(9 7 5 3 1)
 
 ;Задача 28
 ;Определите функцию, вычисляющую, сколько всего атомов в списке.
 
 (defun atm (lst &optional (kol 0))
    ((lambda (x) (and(setq first (car x))(setq last (cdr x)))) lst)
      (cond ((null lst) kol)
            ((atom first) (atm last (+ kol 1)))
            (t (atm last kol))
      )
)

;(atm '(1 2 3 4 (5 6)))
;4
;(atm '((1 2 3)(2 3)))
;0

;Задача 32
;Определить предикат МНОЖЕСТВО, который проверяет, является ли список множеством


(defun mnoj (lst)
	((lambda (x) (and(setq first (car x))(setq last (cdr x)))) lst)
		(cond ((null lst) 'True)
			  ((member first last) 'False)
			  (t (mnoj last))
		)
)

;(mnoj '(1 2 2 3 4 5))
;FALSE
;(mnoj '(4 3 5 2 7 0 1))
;TRUE

;Задача 45
;Напишите функцию (РАССТОЯНИЕ a b), вычисляющую расстояние между городами a b.

(setf (get 'Simf 'x) '-10)
(setf (get 'Simf 'y) '2)
(setf (get 'Bahc 'x) '20)
(setf (get 'Bahc 'y) '-25)
(setf (get 'Sev 'x) '24)
(setf (get 'Sev 'y) '-31)

(defun dist (a b)
    ((lambda (x1 y1 x2 y2) (sqrt(+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1))))) (get a 'x)(get a 'y)(get b 'x)(get b 'y))
)

;(dist 'Bahc 'Sev)
;7.2111025
;(dist 'Simf 'Bahc)
;40.36087 

;Задача 46
;Напишите функцию (РОДИТЕЛИ x) и (СЕСТРЫ_БРАТЬЯ x1 x2).

(setf (get 'Ismail 'dad) 'Ernes)
(setf (get 'Ismail 'mom) 'Elzara)
(setf (get 'Enver 'dad) 'Ernes)
(setf (get 'Enver 'mom) 'Elzara)
(setf (get 'Ernes 'dad) 'Bekir)
(setf (get 'Ernes 'mom) 'Suvade)
(setf (get 'Elvina 'mom) 'Zarema)
(setf (get 'Elvina 'dad) 'Ernes)

(defun parents (x)
    (cons (get x 'dad) (get x 'mom))
)

(defun sis-bro (x1 x2)
    (if (or (eq (get x1 'dad) (get x2 'dad)) (eq (get x1 'mom) (get x2 'mom))) t nil)
)

;(parents 'Enver)
;(ERNES . ELZARA) 
;(sis-bro 'Enver 'Elvina
;T
;(sis-bro 'Enver 'Suvade) 
;NIL



