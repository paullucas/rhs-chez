;; compare :: (Ord a) => a -> a -> Ordering 
(define GT 'GT)
(define LT 'LT)
(define EQ 'EQ)
(define compare
  (lambda (x y)
    (cond ((> x y) GT)
          ((< x y) LT)
          (else EQ))))

