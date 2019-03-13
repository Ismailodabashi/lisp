((defun double (lst &optional (d nil))
    (cond ((null (car lst)) (print (list d)))
          ((eq (member (car lst) (cdr lst)) nil) (double (cdr lst) (cons (car lst) d)))
          (t (double (cdr lst) d))
    )
)

(double '(9 1 3 2 1 2 2 2 3 2 3 4 1 5 4 6 3))