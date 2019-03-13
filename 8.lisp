(defun task (lst &optional (p nil) (n nil))
  (cond ((null (car lst)) (list p n))
        ((> (car lst) 0) (task (cdr lst) (cons (car lst) p) n))
        ((< (car lst) 0) (task (cdr lst) p (cons (car lst) n)))
        ((= (car lst) 0) (task (cdr lst) p n))
  )
) 

(task '(1 2 3 4 0 -2 3 -3))