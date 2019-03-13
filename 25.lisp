(defun chet (lst &optional (n nil))
    (cond ((null (car lst)) (list n))
          ((/= (rem (car lst) 2) 0) (chet (cdr lst) (cons (car lst) n)))
          (t (chet (cdr lst) n))
    )
 )

(car (chet '(9 1 3 2 1 2 2 2 3 2 3 4 1 5 4 6 3))))

