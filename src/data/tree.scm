;; Tree a -> [a]
(define flatten
  (letrec ((f (lambda (t r)
		(cond ((null? t) r)
		      ((pair? t) (f (head t) (f (tail t) r)))
		      (else (cons t r))))))
    (lambda (t)
      (f t nil))))

;; Tree a -> [[a]]
(define levels
  (lambda (t)
    (if (null? t)
	nil
	(let ((lr (partition (compose not pair?) t)))
	  (cons (fst lr) (levels (concat (snd lr))))))))
