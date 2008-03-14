;; data Ordering = LT | EQ | GT
(define lt 'lt)
(define eq 'eq)
(define gt 'gt)

;; compare :: (Ord a) => a -> a -> Ordering 
(define compare
  (lambda (x y)
    (cond ((> x y) gt)
          ((< x y) lt)
          (else eq))))
