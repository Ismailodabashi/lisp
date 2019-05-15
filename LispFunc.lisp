;2.Определите функицонал (MAPLIST fn список) для одного списочного аргумента

(defun maplst (f lst)

          (cond 
             ((or (null f) (null lst)) nil)
             (t(cons (funcall f lst)   (maplst f (cdr lst))))
          )
)

(print (maplst 'list '(1 2 3 4 5)))

;(((1 2 3 4 5)) ((2 3 4 5)) ((3 4 5)) ((4 5)) ((5)))

;4. Определите функциональный предикат(КАЖДЫЙ пред список), который истинен в том и только в том случае, когда, являющейся функциональным аргументом предикат пред истинен для всех элементов списка.

(defun foreach (pred lst)
    (cond 
        ((or (null pred) (null lst)) nil)
        (t  (mapcan #'(lambda (x) 
                              (if (funcall pred x) T NIL)
                      ) lst
            )
        ) 
    )
)

(print (foreach 'evenp  '(2 4 6 8)))

;T

;(print (foreach 'evenp  '(2 4 6 7)))

;NIL

;6. Определите фильтр (УДАЛйЬ-ЕСЛИ пред список), удаляющий из списка список
все элементы, которые обладают свойством, наличие которого проверяет предикат пред.

(defun foreach (pred lst)
    (cond 
        ((or (null pred) (null lst)) nil)
        (t  (mapcan #'(lambda (x) 
                              (if (funcall pred x) T NIL)
                      ) lst
            )
        ) 
    )
)

(defun del-if (pred lst)
    (mapcan #'(lambda(x)
                   (if (eq (foreach pred x) T) nil (list x))) lst)
)

(print (del-if 'evenp  '((2 4 6)(1 2 3)(2 4))))

;((1 2 3))

;8. Напишите генератор натуральных чисел: 0, 1, 2, 3, 4, 5, ...

(defun gen-integer ()
    (let ((x -1)) (lambda () (setq x (+ 1 x))))
)


(setq gen1 (gen-integer))
(setq gen2 (gen-integer))

(print (funcall gen1))
(print (funcall gen1))
(print (funcall gen1))
(print (funcall gen2))
(print (funcall gen1))
(print (funcall gen2))

;0 
;1 
;2 
;0 
;3 
;1 

;10. Напишите генератор, порождающий последовательность (A), (B A), (A B A),
(B A B A), ...

(defun generator ()
	(let ((lst nil)) 
         (lambda ()  
                 (if(eq (car lst) 'A)
                 (setq lst (cons 'B lst))
                 (setq lst (cons 'A lst))
                 )
              
         )
    )
)


(setq gen1 (generator))

(print (funcall gen1))
(print (funcall gen1))
(print (funcall gen1))
(print (funcall gen1))

;(A) 
;(B A) 
;(A B A) 
;(B A B A) 

;12. Определите функцию, которая возвращает в качестве значения свой вызов.

(defun return-self (arg)
    (list 'return-self arg)
)

(print (return-self '(1 2 3)))

;(RETURN-SELF (1 2 3))

;14. Определите функцию, которая возвращает в качестве значения форму своего
определения (DEFUN).

(defun return-form (lst1 lst2)
          `(defun  return-form   (lst1 lst2))
)

(print(return-form '(1 2 3) '(4 5 6)))

;(DEFUN RETURN-FORM (LST1 LST2)) 


