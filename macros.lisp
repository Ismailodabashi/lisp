;1. Определите макрос, который возвращает свой вызов.

(defmacro func (&rest x)
    
   `'(func ,x)     
    
)

(print (func 'a 'b 'c)) 

;(FUNC ('A 'B 'C))

;2.Определите макрос (POP стек), который вычитает из стека верхний элемент и меняет значение переменной стека.

(defmacro pop1 (stack)
  `(let ((first (car ,stack)))(setq ,stack (cdr ,stack))first))

(setq stack '(1 2 3 4 5))
        
        
(print  (pop1 stack))
(print  (pop1 stack))
(print stack)

;1 
;2 
;(3 4 5) 

;3. Определите лисповскую форму (IF условие p q) в виде макроса

(defmacro if1 (cond p l)
    `(if ,cond ,p ,l)
)

(setq x '(1 2 3))
(print (if1 (list x) 'True 'False))
(print (if1 (atom x) 'True 'False))

;TRUE 
;FALSE

;4. Определите в виде макроса форму (FIF тест отр нуль полож).

(defmacro FIF (test n z p)
    `(cond ((< ,test 0) ,n)
           ((> ,test 0) ,p)
           (t ,z)
     )
)

(print (FIF (+ 1 2) 'Negative 'Zero 'Positive))
(print (FIF (- 5 10) 'Negative 'Zero 'Positive))
(print (FIF (* 5 0) 'Negative 'Zero 'Positive))

;POSITIVE 
;NEGATIVE 
;ZERO 

; 5. Определите в виде макроса форму (REPEAT e UNTIL p) паскалевского типа.

(defmacro repeat (e p)

    `(cond(,p nil)

          (t(and(print ,e)(repeat ,e ,p)))

     ) 

)



(setq i 0)
(repeat (setq  i (+ i 1)) (equal i 5))


;1 
;2 
;3 
;4
;5