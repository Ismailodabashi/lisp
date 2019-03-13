(defun atm (lst &optional (kol 0))
  (cond ((null lst) kol)
        ((atom (car lst)) (atm (cdr lst) (+ kol 1)))
        (t (atm (cdr lst) kol))
  )
)

(atm '(1 2 3 4 (5 6)))