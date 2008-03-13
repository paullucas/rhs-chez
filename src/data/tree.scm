;; Tree a -> [a]
(define flatten
  (letrec ((f (lambda (t r)
		(cond ((null? t) r)
		      ((pair? t) (f (head t) (f (tail t) r)))
		      (else (cons t r))))))
    (lambda (t)
      (f t nil))))
