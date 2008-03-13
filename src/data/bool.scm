;; otherwise :: Bool
(define otherwise
  #t)

;; not :: Bool -> Bool
(define not
  (lambda (x)
    (cond ((equal? x #f) #t)
	  ((equal? x #t) #f)
	  (else #f))))
